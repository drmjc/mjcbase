#' are the elements in x all unique?
#' 
#' @inheritParams base::unique
#' @author Mark Cowley, 6 April 2006
#' @export
is.unique <- function(x) {
	return( length(x) == length(unique(x)) )
}
