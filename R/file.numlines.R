#' number of lines in a txt file
#' @param filename path to a text file
#' @return numeric(1)
#' @author Mark Cowley
#' @export
#' @rdname file.numlines
file.numlines <- function(filename) {
	nlines <- NA
	try( {
		cmd <- paste("wc -l", shQuote(filename))
		# cat(cmd, "\n")
		nlines <- system(cmd, intern=TRUE)
		# cat(nlines)
		nlines <- trim(nlines)
		nlines <- sub(" .*", "", nlines)
		nlines <- as.numeric(nlines)
	})
	if( is.na(nlines) )
		nlines <- length(scan.text(filename))
 
    return(nlines)
}

#' @export
#' @rdname file.numlines
numlines <- file.numlines
