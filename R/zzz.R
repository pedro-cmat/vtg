.onLoad <- function(libname, pkgname) {
    # writeln()
    # writeln('vtg .onLoad')
    # writeln(paste('  libname:', libname))
    # writeln(paste('  pkgname:', pkgname))
    # writeln()

    fileName <- system.file('extdata', 'startup.message.txt', package=pkgname)
    msg <- readChar(fileName, file.info(fileName)$size)
    packageStartupMessage(msg)

    log <- lgr::get_logger("vantage/infrastructure")
    assign("log", log, envir=parent.env(environment()))
}

.onAttach <- function(libname, pkgname) {
    # writeln('vtg .onAttach')
    # writeln(paste('  libname:', libname))
    # writeln(paste('  pkgname:', pkgname))
    # writeln()
}

