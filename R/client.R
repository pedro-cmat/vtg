# Libraries
library(httr)
library(rjson)

# ******************************************************************************
# ---- class Client ----
# ******************************************************************************

#' Client for the Vantage federated learning infrastructure
#'
#' Simplifies communication with the REST-api by handling authentication and
#' offering methods for GET/PUT/POST requests.
#'
#' @param host hostname (may include port, separated using a colon, e.g. 'https://localhost:8000')
#' @param username username
#' @param password password
#' @param collaboration_id collaboration ID to post tasks to
#' @param api_path alternative base path, is appended to host
#' @return Client
#' @export
#' @examples
#' client <- Client(
#'     'https://api-test.distributedlearning.ai',
#'     'admin@example.com',
#'     'password',
#'     1
#' )
#'
#' tasks <- client$GET("/task")
Client <- function(host, username, password, collaboration_id, api_path='') {
  # Function arguments are automatically available in the
  # environment, so no need to assign them explicitly.
  env <- environment()

  # Define the class methods. Attributes are accessed through
  # get/set
  self <- list(
    # Convenient access to the environment
    dict = env,

    # Generic getter
    get = function(x) {
      return(get(x, env))
    },

    # Generic setter
    set = function(x, value) {
      assign(x, value, env)
    },

    # Authenticate with the server; sets the access and refresh tokens.
    authenticate = function() {
      # Create the URL and data for the JSON body
      # url <- paste(env$host, env$api_path, '/token', sep='')
      url <- paste(env$host, env$api_path, '/token/user', sep='')

      data <- list(
        username=env$username,
        password=env$password
      )

      r <- POST(url, body=data, encode="json")

      if (r$status_code != 200) {
        stop(sprintf("Could not authenticate: %s", http_status(r)$message))
      }

      # Apparently we were succesful. Retrieve the details from the server
      # response.
      response_data <- content(r)
      list2env(response_data, env)

      return("OK")
    },

    # Refresh the access token using the refresh token
    refresh_token = function() {
      if (is.null(env$refresh_url)) {
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
      response_data <- content(r)
      list2env(response_data, env)

      return("OK")
    },

    # Perform a request to the server
    request = function(method, path, data=NULL, first_try=T) {
      url <- paste(env$host, env$api_path, path, sep='')
      token <- sprintf('Bearer %s', env$access_token)

      if (method == 'GET') {
        r <- GET(url, add_headers(Authorization=token))

      } else if (method == 'POST') {
        r <- POST(url, body=data, encode="json", add_headers(Authorization=token))

      } else if (method == 'PUT') {
        r <- PUT(url, body=data, encode="json", add_headers(Authorization=token))

      }

      if (r$status_code != 200) {
        msg <- sprintf("Request unsuccesful: %s", http_status(r)$message)

        if (first_try) {
          writeln(msg)
          writeln("Refreshing token ... ")
          self$refresh_token()

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

    # Create a data structure used as input for a call to the distributed
    # learning infrastructure.
    create_task_input = function(method, ...) {
        # Construct the input_data list from the ellipsis.
        arguments <- list(...)

        if (is.null(names(arguments))) {
            args <- arguments
            kwargs <- list()

        } else {
            args <- arguments[names(arguments) == ""]
            kwargs <- arguments[names(arguments) != ""]
        }

        # Serialize the argument values to ASCII
        fp <- textConnection("arg_data", open="w")
        saveRDS(args, fp, ascii=T)
        close(fp)

        # Serialize the keyword argument values to ASCII
        fp <- textConnection("kwarg_data", open="w")
        saveRDS(kwargs, fp, ascii=T)
        close(fp)

        # Create the data structure
        input_data <- list(
            method=method,
            args=arg_data,
            kwargs=kwarg_data
        )

        return(input_data)
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

        while(TRUE) {
            r <- self$GET(path)

            if (content(r)$complete) {
                break

            } else {
                # Wait 30 seconds
                writeln("Waiting for results ...")
                Sys.sleep(5)
            }
        }

        path = sprintf('/task/%s?include=results', task$id)
        r <- self$GET(path)

        return(content(r))
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
    call = function(image, method, ...) {
        # Create the json structure for the call to the server
        input <- self$create_task_input(method, ...)

        task = list(
            "name"="",
            "image"=image,
            "collaboration_id"=self$get("collaboration_id"),
            "input"=input,
            "description"=""
        )

        # Create the task on the server; this returs the task with its id
        r <- self$POST('/task', task)

        # Wait for the results to come in
        result_dict <- self$wait.for.results(content(r))

        # result_dict is a list with the keys _id, id, description, complete, image,
        # collaboration, results, etc. the entry "results" is itself a list with
        # one entry for each site. The site's actual result is contained in the
        # named list member 'result' and is encoded using saveRDS.
        sites <- result_dict$results
        return(self$process.results(sites))
    },

    # Return a string representation of this Client
    repr = function() {
      return(sprintf("Client(host='%s', username='%s')", env$host, env$username))
    }
  )

  # Set the class name
  class(self) <- append(class(self), "Client")

  # Return the new object
  return(self)
}



