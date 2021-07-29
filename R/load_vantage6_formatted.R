#' Below you find helper functions to convert either JSON or RDS input to input
#' data for the algorithm.

library(hexView)

DATA_FORMAT_SEPARATOR = '.'
MAX_FORMAT_STRING_LENGTH = 10

load_vantage6_formatted <- function(rawbindata){
    #' Retrieves the input data for the algorithm from the input file received
    #' from the vantage6 infrastructure. Supported data formats are JSON and
    #' RDS.
    #' :param file: input file received from vantage6 infrastructure.
    #' :return: * input data for the algorithm.

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

