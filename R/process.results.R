process.results = function(sites) {
    results <- list()
    errors <- c()

    for (k in 1:length(sites)) {
        writeln(paste('Reading results for site', k))
        results[[k]] <- readRDS(textConnection(sites[[k]]$result))

        if ("error" %in% names(results[[k]]))  {
            node <- sites[[k]]$node
            error <- results[[k]]$error
            msg <- sprintf("Node '%s' returned an error: '%s'", node, error)

            writeln(msg)
            errors <- c(errors, msg)
        }
    }

    if (length(errors)) {
        stop(paste(errors, collapse='\n  '))
    }

    return(results)
}

