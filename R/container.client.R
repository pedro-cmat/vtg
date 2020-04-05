ContainerClient <- R6::R6Class(
    "ContainerClient",
    inherit=Client,
    public = list(

        initialize = function(host, token, api_path) {
            super$initialize(host, api_path=api_path)
            self$access_token = token
        }

    )
)