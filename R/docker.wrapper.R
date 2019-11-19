#' Entrypoint when excecuting this script using Rscript within a Docker container.
#'
#' This function expects the environment variable `DATABASE_URI` to be set and
#' pointing towards a CSV file that can be read by the R-function `read.csv()`
#' _without_ specifying format-details.
#'
#' Performs the following actions:
#' * Load data from the CSV file
#' * Loads the input parameters provided to the container in `input.txt`.
#' * Wraps the docker input/output for `dispatch.RPC()`.
#'
#' (de)serialization is performed in `dispatch.RPC()` to enable testing.
#'
#' @return `null`; output will be written to `output.txt`
docker.wrapper <- function() {
    # TODO: Consider wrapping function body in a tryCatch clause.
    #       This way any issues with dispatching can be more easily debugged.

    database_uri <- Sys.getenv("DATABASE_URI")
    writeln(sprintf("Using '%s' as database", database_uri))
    df <- read.csv(database_uri)

    # Read the contents of file input.txt into 'input_data'
    writeln("Loading input.txt")
    filename <- 'input.txt'
    input_data <- readChar(filename, file.info(filename)$size)

    writeln("Dispatching ...")
    result <- dispatch.RPC(df, input_data)

    # Write result to disk
    writeln("Writing result to disk .. ")
    writeLines(result, "output.txt")

    writeln("")
    writeln("[DONE!]")
}
