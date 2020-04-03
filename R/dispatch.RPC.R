#' Dispatch a remote procedure call.
#'
#' @param df data frame containing the *local* dataset
#' @param input string containing serialized JSON; JSON should contain
#'              the keys `method`, `args` and `kwargs`.
#'
#' @return Output depends on RPC call.
dispatch.RPC <- function(df, input, pkg='') {
    # Determine which method was requested and combine arguments and keyword
    # arguments in a single variable

    # input <- rjson::fromJSON(input)
    method <- sprintf("RPC_%s", input$method)
    #
    # input$args <- readRDS(textConnection(input$args))
    # input$kwargs <- readRDS(textConnection(input$kwargs))

    args <- c(list(df), input$args, input$kwargs)

    # Call the method
    writeln(sprintf("Calling %s", method))

    result <- tryCatch({
        if (pkg == '') {
            result <- list(result=do.call(method, args))
        } else {
            rpc <- eval(parse(text=sprintf("%s::%s", pkg, method)))
            result <- list(result=do.call(rpc, args))
        }

    }, error = function(e) {
        error_msg <- e$message
        writeln(glue::glue('ERROR encountered while calling "{method}": {error_msg}'))
        return(list(error=error_msg))

    })

    # Serialize the result
    # writeln("Serializing result")
    # fp <- textConnection("result_data", open="w")
    # saveRDS(result, fp, ascii=T)
    # close(fp)
    # result <- result_data
    # writeln("Serializing complete")

    return(result)
}

