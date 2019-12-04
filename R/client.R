Client <- R6::R6Class(
    "Client",
    public = list(
        # Attributes
        host = NULL,
        username = NULL,
        password = NULL,
        collaboration_id = NULL,
        api_path = NULL,

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
                stop(sprintf("Could not authenticate: %s", http_status(r)$message))
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

        getCollaborations = function() {
            user <- httr::content(self$GET(self$user_url))

            organization <- httr::content(
                self$GET(sprintf('/organization/%i', user$organization))
            )

            collaborations <- list()

            for (collab_url in organization$collaborations) {
                collaboration <- httr::content(self$GET(collab_url))
                collaborations[[as.character(collaboration$id)]] <- collaboration$name
            }

            collaborations <- data.frame(unlist(collaborations))
            collaborations <- cbind(id=rownames(collaborations), collaborations)
            colnames(collaborations) <- c('id', 'name')

            return(collaborations)
        },

        setCollaborationId = function(collaboration_id) {
            self$collaboration_id <- collaboration_id
        },

        # Refresh the access token using the refresh token
        refresh.token = function() {
            if (is.null(self$refresh_url)) {
                stop("Not authenticated!")
            }

            url <- paste(env$host, env$refresh_url, sep='')
            token <- sprintf('Bearer %s', env$refresh_token)

            r <- POST(url, add_headers(Authorization=token))

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
        request = function(method, path, data=NULL, first_try=T) {
            url <- paste(self$host, self$api_path, path, sep='')
            token <- sprintf('Bearer %s', self$access_token)

            self$log$debug("request:", method=method, url=url)

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
        GET = function(path) {
            return(self$request("GET", path))
        },

        # Perform a POST request to the server
        POST = function(path, data=NULL) {
            return(self$request("POST", path, data))
        },

        # Perform a PUT request to the server
        PUT = function(path, data=NULL) {
            return(self$request("PUT", path, data))
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

            path = sprintf('/task/%s', task$id)

            # Create the progress bar
            pb <- progress::progress_bar$new(
                format="  waiting for results for task ':task' in :elapsed",
                clear=FALSE,
                total=1e7,
                width=60
            )

            while(TRUE) {
                r <- self$GET(path)

                if (httr::content(r)$complete) {
                    break

                } else {
                    # writeln("Waiting for results ...")
                    pb$tick(tokens=list(task=path))
                    Sys.sleep(1)
                }
            }

            # Finish the progress bar
            pb$tick(1e7, tokens=list(task=path))

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
        #   client: ptmclient::Client instance.
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

            task = list(
                "name"=self$task.name,
                "image"=self$image,
                "collaboration_id"=self$collaboration_id,
                "input"=input,
                "description"=""
            )

            # Create the task on the server; this returs the task with its id
            r <- self$POST('/task', task)
            task <- httr::content(r)

            writeln(sprintf('Task has been assigned id %i', task$id))

            # Wait for the results to come in
            result_dict <- self$wait.for.results(task)

            # result_dict is a list with the keys _id, id, description, complete, image,
            # collaboration, results, etc. the entry "results" is itself a list with
            # one entry for each site. The site's actual result is contained in the
            # named list member 'result' and is encoded using saveRDS.
            sites <- result_dict$results
            return(process.results(sites))
        },

        # Return a string representation of this Client
        repr = function() {
            return(sprintf("Client(host='%s', username='%s')", env$host, env$username))
        }
    ),

    private = list(

    )
)