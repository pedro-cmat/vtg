# Create a data structure used as input for a call to the distributed
# learning infrastructure.
create.task.input.unserialized = function(use.master.container, method, ...) {
    log <- lgr::get_logger("vantage/infrastructure/client")

    if (use.master.container) {
        log$debug('Using master container!')
    } else {
        log$debug('Regular call')
    }

    # Construct the input_data list from the ellipsis.
    arguments <- list(...)

    if (is.null(names(arguments))) {
        args <- arguments
        kwargs <- list()

    } else {
        args <- arguments[names(arguments) == ""]
        kwargs <- arguments[names(arguments) != ""]
    }

    # Create the data structure
    input_data <- list(
        master=use.master.container,
        method=method,
        args=args,
        kwargs=kwargs
    )

    log$debug('Created input:')
    log$debug(jsonlite::toJSON(input_data, pretty=T, auto_unbox=T))

    return(input_data)
}
