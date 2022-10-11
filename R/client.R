#' @docType class
#' @title Client for the vantage6 infrastructure.
#' @name Client
#'
#' @description
#' Facilitates querying collaborations, creating tasks and retrieving
#' results.
#'
#' @examples
#' client <- vtg::Client$new('http://localhost:5000', api_path='/api')
#' client$authenticate('root', 'password')
#'
#' collaborations <- client$getCollaborations()
#' print(collaborations)
#'
#' client$set.task.image(image.name, task.name="colnames")
#' result <- client$call("colnames")
#'
#'
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
        use.master.container = F,
        using_encryption = F,
        privkey = NULL,
        SEPARATOR = "$",
        data_format = NULL,
        organizations = NULL,

        log = NULL,

        #' @param host [character()] Host to connect to
        #' @param username [character()] (optional) Username
        #' @param password [character()] (optional) Password
        #' @param collaboration_id [integer()] (optional) Collaboration id
        #' @param organizations [integer()] (optional) organization ids that are within a collaboration
        #' @param api_path [character()] (optional) API path
        #' @param log_level [character()] (optional) Logger level
        initialize = function(host, username='', password='', collaboration_id=NULL, organizations=NULL, api_path='', log_level='info') {
            self$host <- host
            self$username <- username
            self$password <- password
            self$collaboration_id <- collaboration_id
            self$organizations = organizations
            self$api_path <- api_path
            self$log <- lgr::get_logger_glue("vtg/Client")
            lgr::basic_config(threshold = log_level)

            url <- glue::glue('{host}{api_path}/version')
            r <- httr::GET(url)
            self$version <- httr::content(r)$version

            api_version <- self$getVersion()
            self$log$debug('Using API version: {api_version}')
        },

        #' @param username character (optional) Username. If not provided, the
        #'   username and password provided to the constructor are used.
        #' @param password character (optional) Password. Required if username
        #'   is provided.
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

            r <- httr::POST(url, body=data, encode="json")

            if (!is.element(r$status_code, c(200,201,202))) {
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

        setPrivateKey = function(bytes_or_filename) {
            self$privkey <- openssl::read_pem(bytes_or_filename)[["RSA PRIVATE KEY"]]
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
            j <- 1
            for (collab in organization$collaborations) {
                # self$log$debug(glue::glue('Processing collaboration {collab$id}'))
                collaboration_id <- as.character(collab$id)
                endpoint <- glue::glue('/collaboration/{collaboration_id}')
                collaboration <- httr::content(self$GET(collab$link, prefix.api.path=F))

                organization_ids <- c()
                for (org in collaboration$organizations) {
                    organization_ids <- append(organization_ids, org$id)
                }


                collaborations[[j]] <- list(id=collaboration_id,
                                            name=collaboration$name,
                                            organizations=organization_ids)
                j <- j + 1
            }
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

        setOrganizations = function(organizations) {
            self$organizations = organizations
        },

        setCollaborationId = function(collaboration_id) {
            self$collaboration_id <- collaboration_id
            self$collaboration <- self$getCollaboration(collaboration_id)
             self$setUseEncryption(self$collaboration$encrypted)

            for (orgnr in 1:length(self$collaboration$organizations)) {
                org <- self$collaboration$organizations[[orgnr]]
                endpoint <- glue::glue('/organization/{org$id}')
                organization <- httr::content(self$GET(endpoint))

                # Decode the base64-encoded public key
                decoded <- base64enc::base64decode(organization$public_key)
                organization$public_key <- rawToChar(decoded)

                self$collaboration$organizations[[orgnr]] <- organization
            }
        },

        setUseEncryption = function(flag) {
            self$using_encryption <- flag
        },

        setUseMasterContainer = function(flag=T) {
            self$use.master.container = flag
        },

        # Refresh the access token using the refresh token
        refresh.token = function() {
            if (is.null(self$refresh_url)) {
                stop("Not authenticated!")
            }

            url <- paste(self$host, self$refresh_url, sep='')
            token <- sprintf('Bearer %s', self$refresh_token)

            r <- httr::POST(url, httr::add_headers(Authorization=token))

            if (!is.element(r$status_code, c(200,201,202))) {
                stop("Could not refresh token!?")
            }

            # Apparently we were succesful. Retrieve the details from the server
            # response, which includes the key "access_token".
            response_data <- httr::content(r)
            self$access_token <- response_data$access_token

            return("OK")
        },

        # Perform a request to the server
        request = function(method, path, data=NULL, first_try=T, prefix.api.path=T) {
            self$log$debug(glue::glue("method={method}"))
            self$log$debug(glue::glue("path={path}"))
            self$log$debug(glue::glue("prefix_path={prefix.api.path}, api_path={self$api_path}"))
            self$log$debug(glue::glue("host={self$host}"))

            if (prefix.api.path) {
                url <- paste(self$host, self$api_path, path, sep='')
            } else {
                url <- paste(self$host, path, sep='')
            }

            token <- sprintf('Bearer %s', self$access_token)

            self$log$debug("request:", method=method, url=url)

            if (method == 'GET') {
                r <- httr::GET(url, httr::add_headers(Authorization=token))

            } else if (method == 'POST') {
                r <- httr::POST(url, body=data, encode="json", httr::add_headers(Authorization=token))

            } else if (method == 'PUT') {
                r <- httr::PUT(url, body=data, encode="json", httr::add_headers(Authorization=token))

            }

            if (!is.element(r$status_code, c(200,201,202))) {
                msg <- sprintf("Request to '%s' was unsuccessful: %s", url, httr::http_status(r)$message)
                self$log$debug(httr::content(r)$msg)

                if (first_try) {
                    self$log$error(msg)
                    self$log$warn("Refreshing token ... ")
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

            path = sprintf('/task/%s/result', task$id)
            r <- self$GET(path)

            return(httr::content(r))
        },

        decrypt.result = function(serialized.output) {
            parts <- unlist(strsplit(serialized.output, self$SEPARATOR, fixed=T))

            encrypted.key <- openssl::base64_decode(parts[1])
            iv <- openssl::base64_decode(parts[2])
            encrypted.msg <- openssl::base64_decode(parts[3])

            # Decrypt the encrypted key
            key <- openssl::rsa_decrypt(encrypted.key, self$privkey)

            # Use the shared key and iv to decrypt the payload
            serialized.output <- openssl::aes_ctr_decrypt(encrypted.msg, key, iv)
        },

        process.results = function(site_results) {
            results <- list()
            errors <- c()

            num.results <- length(site_results)
            vtg::log$info("Received {num.results} results.")

            for (k in 1:length(site_results)) {
                self$log$debug('Processing result for site {k} (organization_id={site_results[[k]]$organization})')
                self$log$debug(paste("Log:\n", site_results[[k]]$log, sep="", collapse=""))

                marshalled.result <- tryCatch({
                    serialized.output <- site_results[[k]]$result

                    if (self$using_encryption) {
                        self$log$debug('Decrypting result')
                        # Retrieve the components key, iv and msg from the string
                        parts <- unlist(strsplit(serialized.output, self$SEPARATOR, fixed=T))

                        encrypted.key <- openssl::base64_decode(parts[1])
                        iv <- openssl::base64_decode(parts[2])
                        encrypted.msg <- openssl::base64_decode(parts[3])

                        # Decrypt the encrypted key
                        key <- openssl::rsa_decrypt(encrypted.key, self$privkey)

                        # Use the shared key and iv to decrypt the payload
                        serialized.output <- openssl::aes_ctr_decrypt(encrypted.msg, key, iv)

                    } else {
                        self$log$debug('Decoding base64 encoded result')
                        serialized.output <- openssl::base64_decode(serialized.output)
                    }

                    # mimic hexview object
                    rawBlock <- list(width=NULL, offset=0, nbytes=length(serialized.output),
                                      fileRaw=serialized.output, fileNum=NULL,
                                      machine="hex", type="char",
                                      size=1, endian=.Platform$endian, signed=TRUE)
                    class(rawBlock) <- "rawBlock"
                    self$log$debug('format back')
                    marshalled.result <- load_vantage6_formatted(rawBlock)

                    # This has to be the last statement, is the returned value
                    marshalled.result

                }, error = function(e) {
                    self$log$error("could not read results:")
                    self$log$error('Site results:')
                    print(site_results[[k]])
                    self$log$error(jsonlite::toJSON(site_results[[k]], pretty=T, auto_unbox=T))
                    self$log$error('')
                    self$log$error(e)
                })


                if ("error" %in% names(marshalled.result))  {
                    self$log$error('Shoot :@')
                    node <- site_results[[k]]$node
                    error <- marshalled.result$error
                    msg <- sprintf("Node '%s' returned an error: '%s'", node, error)

                    self$log$error(msg)
                    errors <- c(errors, msg)

                } else {
                    results[[k]] <- marshalled.result
                }
            }

            if (length(errors)) {
                stop(paste(errors, collapse='\n  '))
            }

            return(results)
        },

        set.task.image = function(image, task.name='') {
            self$image <- image
            self$task.name <- task.name
        },

        # Encrypt data using an organization's public key.
        #
        # Returns a string containing 3 base64 encoded components, separated by
        # a '$':
        #   1. (RSA) encrypted key,
        #   2. initialization vector (iv),
        #   3. (AES) encrypted body
        encrypt = function(data, org) {
            # Generate a shared key (for use with AES)
            passphrase <- openssl::rand_bytes(32)
            key <- openssl::sha256(passphrase)

            # Encrypt the input using AES (symmetric encryption). This returns
            # an encrypted 'message' and an initialization vector (iv).
            encrypted.msg <- openssl::aes_ctr_encrypt(data, key=key)
            iv <- attr(encrypted.msg, "iv")

            # Base64 encode the initialization vector and message individually.
            # Combine them into a single string, separated by a '$'.
            iv <- openssl::base64_encode(iv)
            encrypted.msg <- openssl::base64_encode(encrypted.msg)
            encoded.input = paste(iv, encrypted.msg, sep=self$SEPARATOR)

            # Encrypt the shared key with the organization's public key (using RSA)
            pubkey = openssl::read_pem(org$public_key)[['PUBLIC KEY']]
            encrypted.key <- openssl::rsa_encrypt(key, pubkey)

            # Base64 encode the RSA encrypted key and prepend it to the previously
            # encrypted body.
            encrypted.key = openssl::base64_encode(encrypted.key)
            encrypted.data <- paste(encrypted.key, encoded.input, sep=self$SEPARATOR)

            return(encrypted.data)
        },

        # Execute a method on the federated infrastructure.
        #
        # This entails ...
        #  * creating a task and letting the hubs execute the method
        #    specified in the 'input' parameter
        #  * waiting for all results to arrive
        #  * deserializing each sites' result using readRDS
        #
        # Params:to
        #   method: name of the method to call on the distributed learning
        #           infrastructure
        #   ...: (keyword) arguments to provide to method. The arguments are serialized
        #        using `saveRDS()` by `create_task_input()`.
        #
        # Return:
        #   return value of called method
        call = function(method, ...) {
            # Create a list() that can be used by dispatch.RPC()
            input <- create.task.input.unserialized(self$use.master.container, method, self$data_format, ...)

            serialized.input <- dump_vantage6_formatted(input, self$data_format)

            if (is.null(self$organizations)){
                for (org in self$collaboration$organizations) {
                    self$organizations <- append(self$organizations, org$id)
                }
            }
            # If we're using encryption, we'll need to encrypt the input for each organization
            # individually (using the organization's public key).
            organizations <- c()
            j <- 1
            for (i in 1:length(self$collaboration$organizations)) {
                cur_org <- self$collaboration$organizations[[i]]


                if (cur_org$id %in% self$organizations) {
                    if (self$using_encryption) {

                        # Returns a string containing 3 base64 encoded components, separated by
                        # a '$':
                        #   1: (RSA) encrypted key,
                        #   2: initialization vector (iv),
                        #   3: (AES) encrypted body
                        self$log$debug('Encrypting input for organization', cur_org$id)
                        input <- self$encrypt(serialized.input, cur_org)

                    } else {
                        input <- openssl::base64_encode(serialized.input)
                    }
                    organizations[[j]] <- list(id=cur_org$id, input=input)
                    j <- j + 1
                }

            }
            self$log$debug('input prepared')
            if (length(organizations) == 0) {
                self$log$error('No organizations.. (are your selected organization in your collaboration?)')
                return()
            }


            task = list(
                "name"=self$task.name,
                "image"=self$image,
                "master"=self$use.master.container,
                "collaboration_id"=self$collaboration_id,
                "organizations"=organizations,
                "description"=""
            )

            # Create the task on the server; this returns the task with its id
            r <- self$POST('/task', task)
            task <- httr::content(r)

            self$log$info(sprintf('Task has been assigned id %i', task$id))
            self$log$info(sprintf(' run id %i', task$id))

            # Wait for the results to come in
            # task <- self$wait.for.results(task)
            site_results <- self$wait.for.results(task)

            # task is a list with the following keys:
            #  - _id
            #  - id
            #  - description
            #  - complete
            #  - image
            #  - collaboration
            #  - results
            # The entry "results" is itself a list (dict) with one entry
            # for each site. The site's actual result is contained in the
            # named list member 'result' and is encoded using saveRDS.
            # site_results <- task$results
            if (self$use.master.container) {
                return(self$process.results(site_results)[[1]])
            }

            return(self$process.results(site_results))
        },

        # Return a string representation of this Client
        repr = function() {
            return(glue::glue("vtg::Client(host='{self$host}', username='{self$username}')"))
        }
    ),

    private = list()
)
