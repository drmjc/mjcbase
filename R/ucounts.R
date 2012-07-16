#' How many times does each term in x appear?
#'
#' \code{\link{table}} does a similar job, but does not provide the option
#' of \code{return.unique=FALSE}
#'
#' @param x a character vector
#' @param return.unique logical: if \code{TRUE}, the numeric counts of the \emph{unique} elements in x
#'	are returned (the names indicate the elements in x).
#'	The order of the returned elements is the same order that
#'	\code{unique(x)} returns.
#'	If \code{FALSE}, then return the numeric counts of each element
#'	in x. Useful if you then want to work out which
#'	elements have count > 1 and quickly view those elements.
#'	The order remains the same as x.
#' 
#' @param issorted logical: to improve the speed of this code, the algorithm first sorts the data.
#'	 If it's already sorted, then save time by setting \code{issorted=FALSE}.
#' @param sort logical: whether to sort the result vector from highest counts to lowest?
#' 
#' @return
#' a \code{vector} of counts, named by the elements of x
#' 
#' @examples
#' ucounts(c("a", "a", "b", "c", "c"), TRUE)
#' # a b c
#' # 2 1 2
#' ucounts(c("a", "a", "b", "c", "c"), FALSE)
#' # a a b c c
#' # 2 2 1 2 2
#' ucounts(c("a", "a", "b", "c", "c"), FALSE) > 1
#' #	a	  a		b	  c		c
#' # TRUE  TRUE FALSE  TRUE	 TRUE
#'
#' @author Mark Cowley, 11 May 2006
#' @export
#' @keywords util manip
ucounts <- function(x, return.unique=TRUE, issorted=FALSE, sort=TRUE) {
	original.x <- x

	if( !issorted )
		x <- sort(x)

	ux <- unique(x)

	res <- rep(NA, length(ux))
	names(res) <- ux

	element.starts <- c(match(ux, x), length(x)+1)

	for(i in 1:length(ux)) {
		res[i] <- element.starts[i+1] - element.starts[i]
	}

	#
	# reorder res back to the original order and return
	#
	if( return.unique ) {
		res <- res[match(unique(original.x), names(res))]
		if( sort ) {
			res <- sort(res, decreasing=TRUE)
		}
	}
	else {
		#
		# expand res to be length(x)
		#
		index <- 1
		tmp <- rep(0, length(original.x))
		for(i in 1:length(res)) {
			index2 <- index+res[i]-1
			tmp[c(index:index2)] <- res[i]
			index <- index2 + 1
		}
		order <- order(original.x)
		tmp2 <- rep(0, length(tmp))
		tmp2[order] <- tmp
		res <- tmp2
		names(res) <- original.x
	}

	return( res )
}

## version 1 -- horribly slow!
## ucounts <- function(x) {
##	   ux <- unique(x)
##
##	   res <- rep(1, length(ux))
##	   names(res) <- ux
##
##	   if( length(ux) < length(x) ) {
##		   for(i in 1:length(ux)) {
##			   res[i] <- sum(x==ux[i])
##		   }
##	   }
##
##	   return( res )
## }

## version 2
## ucounts <- function (x, return.unique = TRUE) {
##	   ux <- unique(x)
##	   if (!return.unique) {
##		   res <- rep(NA, length(x))
##		   names(res) <- x
##	   }
##	   else {
##		   res <- rep(NA, length(ux))
##		   names(res) <- ux
##	   }
##	   for (i in names(res)) {
##		   idx <- which(names(res) == i)
##		   if (all(is.na(res[idx])))
##			   res[idx] <- rep(sum(x == i), length(idx))
##	   }
##	   return(res)
## }

