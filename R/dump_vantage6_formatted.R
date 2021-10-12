dump_vantage6_formatted <- function(payload, data_format){

    # Output serialization
    if(!is.null(data_format)){
        if(tolower(data_format) == 'json'){
            serialized.payload = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, force = T))
            writeln("Serializing to JSON.")
        }
        else{
            serialized.payload = serialize(payload, NULL)
            writeln("Serializing to RDS")
        }

        serialized.data_format <- charToRaw(data_format)
        serialized.dot <- charToRaw('.')
        serialized.data <- c(serialized.data_format, serialized.dot, serialized.payload)

    }
    else{
        serialized.data = serialize(payload, NULL)
        writeln("Serializing to RDS")
    }

    return(serialized.data)
}
