#' Create a data structure that can be used as input for [dispatch.RPC()]).
#'
#' Creates a [base::list()] with keys `master`, `debug`, `method`, `args`
#' and `kwargs` that can be used by [dispatch.RPC()], run on a node, to
#' execute a remote procedure call.
#'
#' @param use.master.container logical Indicates whether the task should be run in a
#'   master container.
#' @param method Character vector (string) of the method to run.
#' @param output_format Format of the returned values
#' @param ... Arguments and keyword arguments
#'
#' @return [list()] with keys `master`, `debug`, `method`, `args`, `kwargs`
create.task.input.unserialized = function(use.master.container, method, output_format, ...) {
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
        # use `pairlist` as toJSON parses that to {} istead of []. If kwargs
        # is a [] the Python algorithms will fail.
        kwargs <- pairlist()

    } else {
        args <- arguments[names(arguments) == ""]
        kwargs <- arguments[names(arguments) != ""]
    }

    # Create the data structure
    list(
        master=use.master.container,
        debug=get.option('debug.container'),
        method=method,
        args=args,
        kwargs=kwargs,
        output_format=output_format
    )
}

