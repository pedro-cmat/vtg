# Create a data structure used as input for a call to the distributed
# learning infrastructure.
create.task.input.unserialized = function(use.master.container, method, ...) {

    if (use.master.container) {
        print('Using master container!')
    } else {
        print('Regular call')
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

    writeln('----------------------------------------------------------------------')
    writeln('Created input:')
    print(input_data)
    writeln('----------------------------------------------------------------------')

    return(input_data)
}
