#' Create a data structure that can be used as input for [dispatch.RPC()]).
#'
#' Creates a [base::list()] with keys `master`, `debug`, `method`, `args`
#' and `kwargs` that can be used by [dispatch.RPC()], run on a node, to
#' execute a remote procedure call.
#'
#' @param use.master.container logical Indicates whether the task should be run in a
#'   master container.
#' @param method Character vector (string) of the method to run.
#' @param ... Arguments and keyword arguments
#'
#' @return [list()] with keys `master`, `debug`, `method`, `args`, `kwargs`
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
        debug=get.option('debug.container'),
        method=method,
        args=args,
        kwargs=kwargs
    )

    return(input_data)
}
