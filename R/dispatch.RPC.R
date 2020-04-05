#' Dispatch a remote procedure call.
#'
#' @param df data frame containing the *local* dataset
#' @param input string containing serialized JSON; JSON should contain
#'              the keys `method`, `args` and `kwargs`.
#'
#' @return Output depends on RPC call.
dispatch.RPC <- function(df, input, pkg='', token='') {
    # Determine which method was requested and combine arguments and keyword
    # arguments in a single variable

    if (input$master == T) {
        writeln('Running a *master* container')

        host <- Sys.getenv('HOST')
        port <- Sys.getenv('PORT')
        api_path <- Sys.getenv('API_PATH')
        host <- glue::glue('{host}:{port}')

        writeln(glue::glue('host: {host}{api_path}'))

        writeln("Splitting token")
        strings <- unlist(strsplit(token, ".", fixed=TRUE))
        print(strings)

        JSON <- rawToChar(base64enc::base64decode(strings[2]))
        jwt <- rjson::fromJSON(JSON)
        collaboration_id <- jwt$identity$collaboration_id

        writeln(glue::glue('Working with collaboration_id <{collaboration_id}>'))

        client <- vtg::ContainerClient$new(host, token, api_path=api_path)
        client$setCollaborationId(collaboration_id)

        method <- input$method
        args <- c(client, input$args, input$kwargs)

    } else {
        writeln('Runnign a (mere) regular container.')
        method <- sprintf("RPC_%s", input$method)
        args <- c(list(df), input$args, input$kwargs)
    }

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

    return(result)
}

