#' MockClient
#'
#' Client that mocks running an algorithm on the vantage6 infrastructure.
#' Particularly useful for testing and developing.
MockClient <- R6::R6Class(
    "MockClient",
    public = list(
        # Attributes
        datasets = NULL,
        pkgname = NULL,
        image = NULL,
        task.name = NULL,
        use.master.container = F,
        using_encryption = F,

        data_format = NULL,

        log = NULL,

        # Constructor
        initialize = function(datasets, pkgname) {
            self$datasets <- datasets
            self$pkgname <- pkgname
            self$log = lgr::get_logger_glue("vtg/MockClient")
        },

        # Methods
        authenticate = function(username='', password='') {
            # pass ...
        },

        set.pkgname = function(pkg) {
            self$pkgname <- pkg
        },

        set.task.image = function(image, task.name='') {
            self$image <- image
            self$task.name <- task.name
        },

        # Mock an RPC call to all sites.
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

            self$log$debug(sprintf('** Mocking call to "{method}" **'))
            datasets <- self$datasets
            input <- create.task.input.unserialized(self$use.master.container, method, self$data_format, ...)

            # Create a list to store the responses from the individual sites
            sites <- list()

            # Mock calling the RPC method on each site
            self$log$debug("there are {length(datasets)} datasets .. ")
            for (k in 1:length(datasets)) {
                log <- capture.output(
                    result <- dispatch.RPC(datasets[[k]], input, pkg=self$pkgname)
                )

                self$log$debug("Log for site {k}:")
                self$log$debug(paste(log, collapse="\n"))

                sites[[k]] <- list(
                    result = result,
                    log = log,
                    node = sprintf("/node/%i", k)
                )
            }

            return(self$process.results(sites))
        },

        process.results = function(site_results) {
            results <- list()

            for (k in 1:length(site_results)) {
                results[[k]] <- site_results[[k]][["result"]]
            }

            return(results)
        }
    )
)

