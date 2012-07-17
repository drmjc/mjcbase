#' Union of 2 or more vectors
#' 
#' This function extends the \code{\link[base]{union}} function to allow more than 2
#' vectors to be unioned. In practice, we do not use union at all, but a more
#' optimised algorithm.
#' 
#' Values are returned in the same order that they are first detected when\cr
#' iterating though \dots
#' 
#' @param \dots at least 2 vectors to be unioned
#' @return A vector containing the union of all the input vectors.
#' @author Mark Cowley
#' @seealso \code{\link{intersectN}}, \code{\link{union}}
#' @keywords logic array
#' @examples
#' 
#' a <- letters[1:15]
#' b <- letters[5:20]
#' c <- letters[10:25]
#' unionN(a, b, c)
#' 
#' @export
unionN <- function(...) {
	args <- list(...)
	if( is.list(args) && length(args) == 1 && length(args[[1]]) > 1 )
		args <- args[[1]]
#	  if( length(args) == 1 )
#		  args <- args[[1]]

	stopifnot( length(args) >= 2 )

	# V1 - looses order
	# res <- unique(sort(unlist(args)))
	
	# V2
	res <- unlist(args)
	res <- res[!duplicated(res)]
	
	return( res )
}
# CHANGELOG
# 15/10/07: optimised to not use union at all, just dump all elements
# into a long vector, and sort, unique that vector.
# 2011-11-14
# - fast and retains the original order.
