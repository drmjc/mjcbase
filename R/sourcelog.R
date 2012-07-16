#' Source all of the functions that are in a log file
#' 
#' @param file the path to a file containing \R functions.
#' @return the file will be \code{source()}d into the 
#' global workspace.
#' @author Mark Cowley, 22 Feb 2006
#' @export
sourcelog <- function(file=dir(, pattern=".log")[1]) {
	IN <- file(file, "r")
	OUT <- NULL
	filename <- NULL
	funcname <- NULL
	inFunction <- FALSE

	line <- readLines(IN, 1)
	while(length(line) > 0) {
		if( grepl("[^#]+<- function", line) ) {
			filename <- tempfile()
			funcname <- sub(" *<- +function.*$", "", line)

			OUT <- file(filename, "w")
			write(line, OUT)
			inFunction <- TRUE
		}
		else if( inFunction && grepl("^}", line) ) {
			write(line, OUT)
			close(OUT)
			inFunction <- FALSE

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
