#' @docType class
#' @title Client for master container algorithms on the vantage6 infrastructure.
#' @name ContainerClient
ContainerClient <- R6::R6Class(
    "ContainerClient",
    inherit=Client,
    public = list(

        initialize = function(host, token, api_path) {
            super$initialize(host, api_path=api_path)
            self$access_token = token

            # Extract collaboration/task information from the token
            self$log$info('Extracting task information from JWT token')
            strings <- unlist(strsplit(token, ".", fixed=TRUE))
            JSON <- rawToChar(base64enc::base64decode(strings[2]))
            identity <- rjson::fromJSON(JSON)$identity

            self$log$debug(glue::glue('  Using collaboration
                                      {identity$collaboration_id}'))
            self$setCollaborationId(identity$collaboration_id)

            self$log$debug(glue::glue('  Using image {identity$image}'))
            self$set.task.image(identity$image, "subtask")
        },

        setUseEncryption = function(flag) {
            # Ignoring everything when running in a container.
            self$using_encryption <- F
        }
    )
)
