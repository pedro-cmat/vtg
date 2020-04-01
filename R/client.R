Client <- R6::R6Class(
    "Client",
    public = list(
        # Attributes
        host = NULL,
        username = NULL,
        password = NULL,
        collaboration_id = NULL,
        collaboration = NULL,
        api_path = NULL,
        version = NULL,

        user_url = NULL,
        access_token = NULL,
        refresh_token = NULL,
        refresh_url = NULL,

        image = NULL,
        task.name = NULL,

        log = NULL,

        # Constructor
        initialize = function(host, username='', password='', collaboration_id=NULL, api_path='') {
            self$host <- host
            self$username <- username
            self$password <- password
            self$collaboration_id <- collaboration_id
            self$api_path <- api_path
            self$log <- lgr::get_logger("vantage/infrastructure/client")

            url <- glue::glue('{host}{api_path}/version')
            r <- httr::GET(url)
            self$version <- httr::content(r)$version

            api_version <- self$getVersion()
            self$log$debug(glue::glue('Using API version {api_version}'))

        },

        # Methods
        authenticate = function(username='', password='') {
            # Create the URL and data for the JSON body
            # url <- paste(env$host, env$api_path, '/token', sep='')
            url <- paste(self$host, self$api_path, '/token/user', sep='')

            if (username != '') {
                self$username <- username
                self$password <- password
            }

            data <- list(
                username=self$username,
                password=self$password
            )

            # self$log$debug("authenticate:", username=username, url=url)
            # print(self$log)

            r <- httr::POST(url, body=data, encode="json")

            if (r$status_code != 200) {
                stop(sprintf("Could not authenticate: %s", httr::http_status(r)$message))
            }

            # Apparently we were succesful. Retrieve the details from the server
            # response.
            response_data <- httr::content(r)

            self$user_url <- response_data$user_url
            self$access_token <- response_data$access_token
            self$refresh_token <- response_data$refresh_token
            self$refresh_url <- response_data$refresh_url

            return("OK")
        },

        getVersion = function() {
            if (is.null(self$version)) {
                self$version <- httr::content(self$GET('/version'))$version
            }

            return(self$version)
        },

        getCollaborations = function() {
            user <- httr::content(self$GET(self$user_url, prefix.api.path=F))
            organization_id <- user$organization$id

            self$log$debug(glue::glue('Using organization_id {organization_id}'))

            organization <- self$getOrganization(organization_id)

            collaborations <- list()

            for (collab in organization$collaborations) {
                # self$log$debug(glue::glue('Processing collaboration {collab$id}'))
                collaboration_id <- as.character(collab$id)

                collaboration <- httr::content(self$GET(collab$link, prefix.api.path=F))
                collaborations[[collaboration_id]] <- collaboration$name
            }


            collaborations <- data.frame(unlist(collaborations))
            collaborations <- cbind(id=rownames(collaborations), collaborations)
            colnames(collaborations) <- c('id', 'name')

            return(collaborations)
        },

        getOrganization = function(organization_id) {
            return(httr::content(
                self$GET(sprintf('/organization/%i', organization_id))
            ))
        },

        getCollaboration = function(collaboration_id) {
            return(httr::content(
                self$GET(sprintf('/collaboration/%i', collaboration_id))
            ))
        },

        setCollaborationId = function(collaboration_id) {
            self$collaboration_id <- collaboration_id
            self$collaboration <- self$getCollaboration(collaboration_id)

            for (org in self$collaboration$organizations) {
                organization <- httr::content(self$GET(org$link, prefix.api.path=F))

                # Decode the base64-encoded public key
                decoded <- base64enc::base64decode(organization$public_key)
                organization$public_key <- rawToChar(decoded)

                self$collaboration$organizations[[org$id]] <- organization
            }
        },

        # Refresh the access token using the refresh token
        refresh.token = function() {
            if (is.null(self$refresh_url)) {
                stop("Not authenticated!")
            }

            url <- paste(self$host, self$refresh_url, sep='')
            token <- sprintf('Bearer %s', self$refresh_token)

            r <- httr::POST(url, httr::add_headers(Authorization=token))

            if (r$status_code != 200) {
                stop("Could not refresh token!?")
            }

            # Apparently we were succesful. Retrieve the details from the server
            # response, which includes the key "access_token".
            response_data <- httr::content(r)
            # list2env(response_data, env)

            return("OK")
        },

        # Perform a request to the server
        request = function(method, path, data=NULL, first_try=T, prefix.api.path=T) {
            if (prefix.api.path) {
                url <- paste(self$host, self$api_path, path, sep='')
            } else {
                url <- paste(self$host, path, sep='')
            }

            token <- sprintf('Bearer %s', self$access_token)

            self$log$trace("request:", method=method, url=url)

            if (method == 'GET') {
                r <- httr::GET(url, httr::add_headers(Authorization=token))

            } else if (method == 'POST') {
                r <- httr::POST(url, body=data, encode="json", httr::add_headers(Authorization=token))

            } else if (method == 'PUT') {
                r <- httr::PUT(url, body=data, encode="json", httr::add_headers(Authorization=token))

            }

            if (r$status_code != 200) {
                msg <- sprintf("Request unsuccesful: %s", httr::http_status(r)$message)

                if (first_try) {
                    writeln(msg)
                    writeln("Refreshing token ... ")
                    self$refresh.token()

                    r <- self$request(method, path, data, first_try=F)

                } else {
                    stop(msg)

                }

            }

            return(r)
        },

        # Perform a GET request to the server
        GET = function(path, prefix.api.path=T) {
            return(self$request("GET", path, prefix.api.path=prefix.api.path))
        },

        # Perform a POST request to the server
        POST = function(path, data=NULL, prefix.api.path=T) {
            return(self$request("POST", path, data, prefix.api.path=prefix.api.path))
        },

        # Perform a PUT request to the server
        PUT = function(path, data=NULL, prefix.api.path=T) {
            return(self$request("PUT", path, data, prefix.api.path=prefix.api.path))
        },

        # Wait for the results of a distributed task and return the task,
        # including results.
        #
        # Params:
        #   client: ptmclient::Client instance.
        #   task: list with the key id (representing the task id)
        #
        # Return:
        #   task (list) including results
        wait.for.results = function(task) {
            # options(use_progress_bar=F)
            use_progress_bar <- getOption('vtg.use_progress_bar', T)

            path = sprintf('/task/%s', task$id)

            # Create the progress bar
            if (use_progress_bar) {
                pb <- progress::progress_bar$new(
                    format="  waiting for results for task ':task' in :elapsed",
                    clear=FALSE,
                    total=1e7,
                    width=60
                )
            }


            while(TRUE) {
                r <- self$GET(path)

                if (httr::content(r)$complete) {
                    break

                } else {
                    # writeln("Waiting for results ...")
                    if (use_progress_bar) {
                        pb$tick(tokens=list(task=path))
                    } else {
                        cat('.')
                        flush.console()
                    }

                    Sys.sleep(1)
                }
            }

            if (use_progress_bar) {
                # Finish the progress bar
                pb$tick(1e7, tokens=list(task=path))
            } else {
                writeln('')
            }

            path = sprintf('/task/%s?include=results', task$id)
            r <- self$GET(path)

            return(httr::content(r))
        },

        set.task.image = function(image, task.name='') {
            self$image <- image
            self$task.name <- task.name
        },

        # Execute a method on the distributed learning infrastructure.
        #
        # This entails ...
        #  * creating a task and letting the hubs execute the method
        #    specified in the 'input' parameter
        #  * waiting for all results to arrive
        #  * deserializing each sites' result using readRDS
        #
        # Params:
        #   method: name of the method to call on the distributed learning
        #           infrastructure
        #   ...: (keyword) arguments to provide to method. The arguments are serialized
        #        using `saveRDS()` by `create_task_input()`.
        #
        # Return:
        #   return value of called method
        call = function(method, ...) {
            # self$log$info("calling %s", method)

            # Create the json structure for the call to the server
            input <- create.task.input(method, ...)
            organizations <- c()

            for (i in 1:length(self$collaboration$organizations)) {
                org <- self$collaboration$organizations[[i]]
                organizations[[i]] <- list(id=org$id, input=input)
            }

            task = list(
                "name"=self$task.name,
                "image"=self$image,
                "collaboration_id"=self$collaboration_id,
                "organizations"=organizations,
                "description"=""
            )

            # paste(rep('-', 60), sep='', collapse='')
            # print(task)
            # paste(rep('-', 60), sep='', collapse='')

            # Create the task on the server; this returs the task with its id
            r <- self$POST('/task', task)
            task <- httr::content(r)

            vtg::log$info(sprintf('Task has been assigned id %i', task$id))

            # Wait for the results to come in
            task <- self$wait.for.results(task)

            # task is a list with the following keys:
            #  - _id
            #  - id
            #  - description
            #  - complete
            #  - imag
            #  - collaboration
            #  - results
            # The entry "results" is itself a list (dict) with one entry
            # for each site. The site's actual result is contained in the
            # named list member 'result' and is encoded using saveRDS.
            site_results <- task$results
            return(process.results(site_results))
        },

        # Return a string representation of this Client
        repr = function() {
            return(sprintf("Client(host='%s', username='%s')", env$host, env$username))
        }
    ),

    private = list(

    )
)