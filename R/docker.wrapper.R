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
docker.wrapper <- function(pkg='') {
    # TODO: Consider wrapping function body in a tryCatch clause.
    #       This way any issues with dispatching can be more easily debugged.

    database_uri <- Sys.getenv("DATABASE_URI")
    writeln(sprintf("Using '%s' as database", database_uri))
    df <- read.csv(database_uri)

    # Read the contents of file input.txt into 'input_data'
    input_file <- Sys.getenv("INPUT_FILE")
    writeln(glue::glue("Loading data from '{input_file}'"))
    input_data = read_formatted(input_file)

    # Read the contents of file input.txt into 'input_data'
    token_file <- Sys.getenv("TOKEN_FILE")
    writeln(glue::glue("Loading token from '{token_file}'"))
    token <- readChar(token_file, file.info(token_file)$size)

    writeln("Dispatching ...")
    result <- dispatch.RPC(df, input_data, pkg=pkg, token=token)

    # Write result to disk
    output_file <- Sys.getenv("OUTPUT_FILE")
    writeln(glue::glue("Writing data to '{output_file}'"))

    # Output serialization
    if(!is.null(input_data$output_format)){
        if(tolower(input_data$output_format) == 'json'){
            serialized_result = charToRaw(jsonlite::toJSON(result, auto_unbox = TRUE))
            writeln("Serializing output to JSON.")
        }
        else{
            serialized_result = serialize(result, NULL)
            writeln("Serializing output to RDS")
        }
    }
    else{
        serialized_result = serialize(result, NULL)
        writeln("Serializing output to RDS")
    }

    writeBin(serialized_result, output_file)


    writeln("")
    writeln("[DONE!]")
}

#' Below you find helper functions to convert either JSON or RDS input to input
#' data for the algorithm.

library(hexView)

DATA_FORMAT_SEPARATOR = '.'
MAX_FORMAT_STRING_LENGTH = 10

read_formatted <- function(input_file){
    #' Retrieves the input data for the algorithm from the input file received
    #' from the vantage6 infrastructure. Supported data formats are JSON and
    #' RDS.
    #' :param file: input file received from vantage6 infrastructure.
    #' :return: * input data for the algorithm.

    rawbindata <- hexView::readRaw(input_file)
    splitted_data <- read_data_format(rawbindata)
    data_format <- splitted_data$data_format
    dot_location <- splitted_data$dot_location

    #supported data formats for the R client: json, rds
    if((is.element(data_format, list('json', 'rds')))){
        if(data_format == 'json'){
            input_data = read_json_data(rawbindata, dot_location)
        } else if(data_format == 'rds'){
            input_data = read_rds_data(rawbindata, dot_location)
        }
    } else {
        stop(sprintf('%s is not supported.', data_format))
    }
    return(input_data)
}

read_data_format <- function(rawbindata){
    #' Try to read the prescribed data format. The data format should be
    #' specified as follows: DATA_FORMAT.ACTUAL_BYTES. This function will
    #' attempt to read the string before the period. It will warn the user if
    #' the file is doesn't have a supported data format prepended. In this case
    #' it will continue assuming it is RDS.
    #' :param file: raw binary data.
    #' :return: * data format ('json' or 'rds'),
    #'          * period/dot location which is needed to retrieve the data.

    data_format <- list()
    for(i in seq(MAX_FORMAT_STRING_LENGTH)){
        char <- hexView::blockValue(rawbindata)[i]
        if( char != DATA_FORMAT_SEPARATOR){
            data_format[[i]] <- char
        } else {
            data_format <- tolower(paste(data_format, collapse=''))
            break
        }
    }
    if((!is.element(data_format, list('json', 'rds')))){
        warning('No supported data format prepended. Proceding, assuming it is RDS.')
        i = 0
        data_format = 'rds' # default data format if none specified
    }
    return(list("data_format" = data_format, "dot_location" = i))
}

read_json_data <- function(rawbindata, dot_location){
    #' Translate the JSON data to an R object.
    #' :param file: raw binary data, and the period/dot location.
    #' :return: * input data for the algorithm.

    data <- list()
    for(j in seq(from=(dot_location+1), to=rawbindata$nbytes)){
        char <- hexView::blockValue(rawbindata)[j]
        data[[(j-dot_location)]] <- char
    }
    json_data <- paste(data, collapse='')
    data <- jsonlite::fromJSON(json_data)
    return(data)
}

read_rds_data <- function(rawbindata, dot_location){
    #' Translate the RDS data to an R object.
    #' :param file: raw binary data, and the period/dot location.
    #' :return: * input data for the algorithm.

    rds_data <- rawbindata$fileRaw[(dot_location+1):length(rawbindata$fileRaw)]
    data <- readRDS(gzcon(rawConnection(rds_data)))
    return(data)
}
