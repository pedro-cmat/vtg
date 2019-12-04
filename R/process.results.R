process.results = function(site_results) {
    results <- list()
    errors <- c()

    for (k in 1:length(site_results)) {
        writeln(paste('Reading results for site', k))
        # The RDS encoded result is a list, with *one* of two keys:
        #  - result
        #  - error
        results[[k]] <- readRDS(textConnection(site_results[[k]]$result))

        if ("error" %in% names(results[[k]]))  {
            node <- site_results[[k]]$node
            error <- results[[k]]$error
            msg <- sprintf("Node '%s' returned an error: '%s'", node, error)

            writeln(msg)
            errors <- c(errors, msg)

        } else {
            results[[k]] <- results[[k]]$result

        }

    }

    if (length(errors)) {
        stop(paste(errors, collapse='\n  '))
    }

    return(results)
}

