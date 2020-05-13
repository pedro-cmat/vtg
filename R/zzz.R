.onLoad <- function(libname, pkgname) {
    # writeln()
    # writeln('vtg .onLoad')
    # writeln(paste('  libname:', libname))
    # writeln(paste('  pkgname:', pkgname))
    # writeln()

    fileName <- system.file('extdata', 'startup.message.txt', package=pkgname)
    msg <- readChar(fileName, file.info(fileName)$size)
    packageStartupMessage(msg)

    log <- lgr::get_logger_glue("vantage/infrastructure")
    assign("log", log, envir=parent.env(environment()))

    set.option("debug.container", FALSE)
}

.onAttach <- function(libname, pkgname) {
    # writeln('vtg .onAttach')
    # writeln(paste('  libname:', libname))
    # writeln(paste('  pkgname:', pkgname))
    # writeln()
}

options <- new.env()

set.option <- function(name, value) {
    assign(name, value, envir=options)
}

get.option <- function(name) {
    return(get(name, envir=options))
}
