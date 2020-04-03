process.results = function(site_results) {
    results <- list()
    errors <- c()

    for (k in 1:length(site_results)) {
        vtg::log$trace(paste('Reading results for site', k))
        # The RDS encoded result is a list, with *one* of two keys:
        #  - result
        #  - error

        results[[k]] <- tryCatch({


            readRDS(textConnection(site_results[[k]]$result))
        }, error = function(e) {
            writeln("could not read results:")
            writeln(site_results[[k]])
        })

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

