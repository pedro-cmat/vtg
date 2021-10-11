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
            identity = jose::jwt_split(token)$payload$identity
            self$setCollaborationId(identity$collaboration_id)
            self$set.task.image(identity$image, "subtask")
        },

        setUseEncryption = function(flag) {
            # Ignoring everything when running in a container.
            self$using_encryption <- F
        }
    )
)
