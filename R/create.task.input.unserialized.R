# Create a data structure used as input for a call to the distributed
# learning infrastructure.
create.task.input.unserialized = function(method, ...) {
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
        method=method,
        args=args,
        kwargs=kwargs
    )

    return(input_data)
}
