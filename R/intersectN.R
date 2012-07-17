#' Intersection of multiple vectors
#' This function extends \code{\link{intersect}} which only accepts 2 vectors,
#' to allow many vectors.
#' 
#' This method is nicely optimised at each iteration so that it compares the
#' smallest lists first, and stops if there are no elements in the
#' intersection.
#' 
#' @param \dots The names of various vectors that you wish to intersect.
#' @return A vector of elements that are found in all input vectors.
#' @author Mark Cowley
#' @seealso \code{\link{intersect}}
#' @keywords logic array
#' @examples
#' a <- letters[1:15]
#' b <- letters[5:20]
#' c <- letters[10:25]
#' intersectN(a, b, c)
#' 
#' @export
intersectN <- function(...) {
    args <- list(...)
    if( is.list(args) && length(args) == 1 && length(args[[1]]) > 1 )
    	args <- args[[1]]

    stopifnot( length(args) >= 2 )

    # makes sense to compare the shortest things in order
    args <- args[order(sapply(args,length), decreasing=TRUE)]
    
    res <- intersect(args[[1]], args[[2]])
    if(length(args) > 2) {
        for(i in 3:length(args)) {
            res <- intersect(res, args[[i]])
            if(length(res) == 0)
                return( numeric(0) )
        }
    }
    return( res )
}
# CHANGELOG
# 15/10/07: MJC optimised method to look for intersections in order in
# increasing length lists, ie if there are 5 things to compare, and 1
# of them has only 3 elements, then no point finding the intersections
# in the longer lists when the upper limit on the number of items intersected
# is the length of the shortest list.
