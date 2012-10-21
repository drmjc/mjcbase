#' sorting or ordering more complex objects
#'
#' @inheritParams base::sort
#' @param FUN a sort function. if \code{x} is 2D or more, then this is applied to the 1st
#' dimension (ie the rows)
#' 
#' @return a sorted object of same type as \code{x}
#' 
#' @author Mark Cowley
#' @exportMethod sort
#' @rdname sort-methods
#' @docType methods
setGeneric("sort", function(x, decreasing=FALSE, na.last=NA, FUN, ...) standardGeneric("sort"))

#' @rdname sort-methods
#' @aliases sort,matrix-method
setMethod(
	"sort",
	signature=signature("matrix"),
	function(x, decreasing=FALSE, na.last=NA, FUN, ...) {
		FUN <- match.fun(FUN)
	    
		val <- apply(x, 1, FUN, ...)
		res <- x[order(val, decreasing=decreasing, na.last=na.last), ]

		return(res)
		
	}
)

#' @rdname sort-methods
#' @aliases sort,data.frame-method
setMethod(
	"sort",
	signature=signature("data.frame"),
	function(x, decreasing=FALSE, na.last=NA, FUN, ...) {
		FUN <- match.fun(FUN)
	    
		cols <- colclasses(x) == "numeric"
		res <- x
		res[,cols] <- sort(as.matrix(x[,cols]), decreasing=decreasing, na.last=na.last, FUN=FUN, ...)

		return(res)
		
	}
)

#' @rdname sort-methods
#' @aliases sort,ANY-method
setMethod(
	"sort",
	signature=signature("ANY"),
	function(x, decreasing=FALSE, na.last=NA, FUN, ...) {
		base::sort(x, decreasing=decreasing, ...)
	}
)
