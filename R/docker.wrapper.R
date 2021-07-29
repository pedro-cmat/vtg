#' @title docker.wrapper()
#'
#' @description
#' This function is run by Docker when a `vtg`-based container is
#' started. It handles communication (input/output) with the Node
#' by reading/writing to files.
#'
#' It expects the following environment variables to be set:
#'  * `DATABASE_URI`: filename of a CSV file that can be read by
#'    the R-function [read.csv()]  _without_ specifying format-details.
#'  * `INPUT_FILE`: filename that contains algorithm input in RDS format.
#'    Input should be a [list()] as created using [create.task.input.unserialized()].
#'  * `OUTPUT_FILE`: filename of file that's used to write results to.
#'  * `TOKEN_FILE`: filename of file contains a JSON Web Token.
#'
#' Performs the following actions:
#' * Load data from the CSV file
#' * Loads the input parameters provided to the container in `input.txt`.
#' * Wraps the docker input/output for `dispatch.RPC()`.
#'
#' @param pkg [character()] with the name of the R package to dispatch to.
#'
#' @return `null`; output will be written to `output.txt`

library(hexView)

docker.wrapper <- function(pkg='') {
    # TODO: Consider wrapping function body in a tryCatch clause.
    #       This way any issues with dispatching can be more easily debugged.

    database_uri <- Sys.getenv("DATABASE_URI")
    writeln(sprintf("Using '%s' as database", database_uri))
    df <- read.csv(database_uri)

    # Read the contents of file input.txt into 'input_data'
    input_file <- Sys.getenv("INPUT_FILE")
    writeln(glue::glue("Loading data from '{input_file}'"))
    rawbindata <- hexView::readRaw(input_file)
    input_data = load_vantage6_formatted(rawbindata)

    # Read the contents of file input.txt into 'input_data'
    token_file <- Sys.getenv("TOKEN_FILE")
    writeln(glue::glue("Loading token from '{token_file}'"))
    token <- readChar(token_file, file.info(token_file)$size)

    writeln("Dispatching ...")
    result <- dispatch.RPC(df, input_data, pkg=pkg, token=token)

    # Write result to disk
    output_file <- Sys.getenv("OUTPUT_FILE")
    writeln(glue::glue("Writing data to '{output_file}'"))

    serialized_result <- dump_vantage6_formatted(result, input_data$output_format)
    writeBin(serialized_result, output_file)

    writeln("")
    writeln("[DONE!]")
}
