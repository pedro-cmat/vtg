#' Fake client
MockClient <- function(datasets) {
    env <- environment()
    self <- list(
        datasets = datasets,

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
        call <- function(client, method, ...) {

            writeln(sprintf('** Mocking call to "%s" **', method))
            datasets <- client$datasets
            input <- create_task_input(method, ...)
            input <- toJSON(input)

            # Create a list to store the responses from the individual sites
            sites <- list()

            # Mock calling the RPC method on each site
            for (k in 1:length(datasets)) {
                sites[[k]] <- list(
                    result = dispatch.RPC(datasets[[k]], input),
                    log = "this was a mocked call",
                    node = sprintf("/node/%i", k)
                )
            }

            return(process.results(sites))
        }
    )

    return(self)
}

