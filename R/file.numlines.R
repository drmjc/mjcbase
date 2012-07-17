
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

numlines <- function(filename) {
    return(file.numlines(filename))
}
