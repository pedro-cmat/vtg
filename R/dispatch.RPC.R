#' Dispatch a remote procedure call.
#'
#' This method is called by [docker.wrapper()], supplying `df`, `input` and
#' `pkg`. Token will be supplied only when running within a master container.
#'
#' @param df [data.frame()] containing the *local* dataset
#' @param input [list()] with at least the keys `method`, `args` and `kwargs`.
#'              Key `debug` ([logical()]) is optional.
#'
#' @return Output depends on RPC call.
dispatch.RPC <- function(df, input, pkg='', token='') {
    # Determine which method was requested and combine arguments and keyword
    # arguments in a single variable

    if (is.null(input[['debug']])) {
        input[['debug']] = F
    }

    if (input$master == T) {
        writeln('Running a *master* container')

        if (input$debug == T) {
            lgr::lgr$set_threshold("debug")

            lgr::lgr$appenders$console$set_layout(
                lgr::LayoutFormat$new(
                    fmt="%t - %L (master container - %c): %m",
                    timestamp_fmt="%H:%M:%S",
                    colors=getOption("lgr.colors"),
                    pad_levels="right"
                )
            )

            writeln('Input:')
            print(input)
        }

        host <- Sys.getenv('HOST')
        port <- Sys.getenv('PORT')
        api_path <- Sys.getenv('API_PATH')
        host <- glue::glue('{host}:{port}')

        writeln(glue::glue('host: {host}{api_path}'))

        # writeln("Manually decoding JWT")
        # strings <- unlist(strsplit(token, ".", fixed=TRUE))
        # print(strings)

        # JSON <- rawToChar(base64enc::base64decode(strings[2]))
        # jwt <- rjson::fromJSON(JSON)

        # collaboration_id <- jwt$identity$collaboration_id

        # if (input$debug & pkg != '') {
        #     tryCatch({
        #         writeln('JWT contents:')
        #         print(jwt)
        #         writeln('')

        #         image.name <- jwt$identity$image
        #         set.image.cmd <- glue::glue("{pkg}::set.option('image.name', '{image.name}')")
        #         print(glue::glue('Setting image name: "{set.image.cmd}"'))
        #         eval(parse(text=set.image.cmd))
        #
        #     }, error=function(e) {
        #         writeln(glue::glue('ERROR: Could not set image name: "{e}"'))
        #     })
        # }

        # writeln(glue::glue('Working with collaboration_id <{collaboration_id}>'))

        client <- ContainerClient$new(host, token, api_path=api_path)
        # client$setCollaborationId(collaboration_id)

        method <- input$method
        args <- c(client, input$args, input$kwargs)

    } else {
        writeln('Running a (mere) regular container.')
        method <- sprintf("RPC_%s", input$method)
        args <- c(list(df), input$args, input$kwargs)
    }

    # Call the method
    writeln(sprintf("Calling %s", method))

    result <- tryCatch({
        if (pkg == '') {
            result <- do.call(method, args)
        } else {
            rpc <- eval(parse(text=sprintf("%s::%s", pkg, method)))
            result <- do.call(rpc, args)
        }

    }, error = function(e) {
        error_msg <- e$message
        writeln(glue::glue('ERROR encountered while calling "{method}": {error_msg}'))
        print(e$call)
        print(e)

        return(list(error=error_msg))

    })

    # writeln('Returning output')
    # print(result)

    return(result)
}

