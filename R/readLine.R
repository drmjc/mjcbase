#' readLine, and split on character
#' 
#' read a single line from a file, or connection, and then
#' split the result on a separator. eg tab, comma, or spaces
#' 
#' @param file a file, or open connection
#' @param split the character to split on. eg tab, comma, or default=spaces
#' @param ok unused
#' 
#' @return a character vector of length >= 0
#' 
#' @author Mark Cowley
#' @export
readLine <- function(file, split=" +", ok=FALSE) {

	tmp <- readLines(file, n=1, ok=TRUE)
	if( length(tmp) == 0 )
		return(tmp)
	else
		return( strsplit( trim(tmp), split )[[1]] )
}

#' skipLine
#' 
#' skip a line from an open file connection
#' 
#' @param file an open connection
#' 
#' @return nothing
#' 
#' @author Mark Cowley
#' @export
skipLine <- function(file) {
	tmp <- readLines(file, n=1, ok=TRUE)
}
