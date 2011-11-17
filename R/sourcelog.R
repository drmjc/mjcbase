## Source all of the functions that are in a log file
##
## Mark Cowley, 22 Feb 2006
##
sourcelog <- function(file=dir(, pattern=".log")[1]) {
    IN <- file(file, "r")
    OUT <- NULL
    filename <- NULL
    funcname <- NULL
    inFunction <- F

    line <- readLines(IN, 1)
    while(length(line) > 0) {
        if( grepT("[^#]+<- function", line) ) {
            filename <- tempfile()
            funcname <- sub(" *<- +function.*$", "", line)

            OUT <- file(filename, "w")
            write(line, OUT)
            inFunction <- T
        }
        else if( inFunction && grepT("^}", line) ) {
            write(line, OUT)
            close(OUT)
            inFunction <- F

            cat( paste("sourcing", funcname, "\n") )
            source(filename)
        }
        else if ( inFunction ) {
            write(line, OUT)
        }

        line <- readLines(IN, 1)
    }

    close( IN )
}
