# Create a data structure used as input for a call to the distributed
# learning infrastructure.
create.task.input = function(method, ...) {
    # Construct the input_data list from the ellipsis.
    arguments <- list(...)

    if (is.null(names(arguments))) {
        args <- arguments
        kwargs <- list()

    } else {
        args <- arguments[names(arguments) == ""]
        kwargs <- arguments[names(arguments) != ""]
    }

    # Serialize the argument values to ASCII
    fp <- textConnection("arg_data", open="w")
    saveRDS(args, fp, ascii=T)
    close(fp)

    # Serialize the keyword argument values to ASCII
    fp <- textConnection("kwarg_data", open="w")
    saveRDS(kwargs, fp, ascii=T)
    close(fp)

    # Create the data structure
    input_data <- list(
        method=method,
        args=arg_data,
        kwargs=kwarg_data
    )

    return(input_data)
}
