#' prettyNum for matrices and tables
#' 
#' Extension of the excellent \code{\link[base]{prettyNum}} from \code{base} to
#'  work with \code{matrix} & \code{data.frame} objects.
#' 
#' \code{prettyNum} is the utility function for prettifying \code{x}.
#' \code{x} can be complex (or \code{format(<complex>)}, \code{matrix} or \code{data.frame} here.
#' If each element of \code{x} is not a character, \code{format(x[i], ...)} 
#' is applied to each element, and
#' then it is left unchanged if all the other arguments are at their
#' defaults.  Note that \code{prettyNum(x)} may behave unexpectedly if elements of \code{x}
#' are a \code{character} vector not resulting from something like
#' \code{format(<number>)}: in particular it assumes that a period is a
#' decimal mark.
#' 
#' @param x a \code{data.frame}, \code{matrix} or \code{vector}
#' @return A character object of same size and attributes as \code{x}, in the
#'     current locale's encoding.
#' @seealso \code{\link[base]{prettyNum}}
#' @author Mark Cowley
#' @export
prettyNum <- function(x) {
	if( is.matrix.like(x) ) {
		val <- base::prettyNum( as.vector(x) )
		res <- matrix(val, nrow(x), ncol(x), byrow=FALSE)
		if( is.data.frame(x) )
			res <- as.data.frame(res)
		dimnames(res) <- dimnames(x)
		
		return( res )
	}
	else {
		base::prettyNum(x)
	}
}
