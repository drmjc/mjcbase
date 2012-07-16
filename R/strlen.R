#' Length of each string in a vector.
#' 
#' This is just a wrapper to nchar
#' 
#' @inheritParams base::nchar
#' @author Mark Cowley, 2010-01-09
#' @export
strlen <- function(x) {
	return(nchar(x))
}
