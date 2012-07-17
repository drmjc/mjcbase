#' Write a list of vectors out to a file
#' 
#' Write a list of vectors out to a file. Each vector will appear on its own line, with
#' each element within that vector separated by \code{sep}.
#' 
#' @param text a list of vectors
#' @param con a connection, or path to a filename
#' @param sep the seperator between elements
#' @param eol the end of line character
#' @param useBytes logical: see Details of \code{\link{writeLines}}
#' @param names logical: if \code{TRUE}, and \code{text} has valid names, write those
#'   names at the start of each line.
#' @return none. write a file.
#' @author Mark Cowley, 2011-08-05
#' @examples
#' x <- list(A=letters, B=LETTERS, C=1:10)
#' f <- tempfile()
#' writeLines.list(x, f)
#' f2 <- tempfile()
#' writeLines.list(x, f2, names=TRUE)
#' @export
writeLines.list <- function(text, con = stdout(), sep="\t", eol = "\n", useBytes = FALSE, names=FALSE) {
	is(text, "list") || stop("text must be a list")
	all(!sapply(text, "class") %in% c("matrix", "data.frame")) || stop("text must be a list of vectors. see write.delim.list")
	
	if(names && is.null(names(text))) names=FALSE
	
	if( is.character(con) ) {
		con <- file(con, "w")
		on.exit(close(con))
	}
	
	for(i in seq(along=text)) {
		line <- text[[i]]
		if(names) line <- c(names(text)[i], line)
		line <- paste(line, collapse=sep)
		writeLines(line, con, sep=eol, useBytes=useBytes)
	}
}

test_writeLines.list <- function() {
	x <- list(A=letters, B=LETTERS, C=1:10)
	writeLines.list(x, "tmp.txt")
	writeLines.list(x, "tmp2.txt", names=TRUE)
}
