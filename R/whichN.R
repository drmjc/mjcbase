#' whichN
#' 
#' @description \code{which.minN}: Get the indices of the N smallest values in x
#'
#' @param x a vector of numbers
#' @param N how many values are to be returned
#' @param return.ties logical: if there are multiple Nth smallest values, return all the indices?
#'
#' @return \code{which.minN}: the indices of the \code{N} smallest values in \code{x}. possibly more than \code{N} if
#'     \code{return.ties=TRUE} and the \code{N}th smallest value appears more than once.
#'
#' @author Mark Cowley, 24 May 2005
#'
#' @export
#' @rdname whichN
which.minN <- function(x, N=5, return.ties=FALSE) {
    res <- (1:length(x))
    if( !is.null(names(x)) )
        names(res) <- names(x)

    res <- res[ order(x, decreasing=FALSE, na.last=TRUE) ]

    if( return.ties ) {
        ## remove the 1:N min elements from x, then see if there are any more elements == the Nth element
        ## If there are, increment N so that they will all be returned.
        N <- N + sum(x[-res[1:N] ] == x[ res[N] ])
        return( res[1:N] )
    }
    else
        return( res[1:N] )
}

#' @description \code{minN}: Get the N smallest values from x
#' 
#' @param sort if \code{FALSE}: return the N values in order of appearance;
#'  else return the N values in increasing order.
#' 
#' @return \code{minN}: return the N smallest values from \code{x} (possibly sorted), possibly
#' more than N if return.ties=TRUE and the Nth smallest value appears more than
#' once.
#' 
#' @export
#' @rdname whichN
minN <- function(x, N=5, sort=TRUE, return.ties=FALSE) {
    if( sort ) {
        x <- sort(x, decreasing=FALSE, na.last=TRUE)
        if( return.ties )
            N <- N + sum(x[-c(1:N)] == x[ x[N] ], na.rm=TRUE)
        return(x[1:N])
    }
    else
        ## although counterintuitive, the sort call in the following sorts the INDICES which
        ## makes the returned values unsorted in reference to the values in x.
        return( x[ sort(which.minN(x, N, return.ties)) ] )
}

#' @description \code{which.maxN}: Get the indices of the N largest values in x
#' 
#' @return \code{which.maxN}: return the indices of the N largest values in x. possibly more than
#' N if return.ties=TRUE and the Nth largest value appears more than once.
#' 
#' @export
#' @rdname whichN
which.maxN <- function(x, N=5, return.ties=FALSE) {
    res <- (1:length(x))
    if( !is.null(names(x)) )
        names(res) <- names(x)

    res <- res[ order(x, decreasing=TRUE, na.last=TRUE) ]

    if( return.ties )
        ## remove the 1:N min elements from x, then see if there are any more elements == the Nth element
        ## If there are, increment N so that they will all be returned.
        N <- N + sum(x[ -res[1:N] ] == x[ res[N] ], na.rm=TRUE)

    return( res[1:N] )
}


#' @description \code{maxN}: Get the N largest values from x
#' 
#' @return \code{maxN}: return the N largest values from x (possibly sorted), possibly more
#' than N if return.ties=TRUE and the Nth largest value appears more than once.
#' @export
#' @rdname whichN
maxN <- function(x, N=5, sort=TRUE, return.ties=FALSE) {
    if(sort) {
        x <- sort(x, decreasing=TRUE, na.last=TRUE)
        if( return.ties )
            N <- N + sum(x[-c(1:N)] == x[ x[N] ], na.rm=TRUE)
        return(x[1:N])
    }
    else
        ## although counterintuitive, the sort call in the following sorts the INDICES which
        ## makes the returned values unsorted in reference to the values in x.
        return( x[ sort(which.maxN(x, N, return.ties), na.rm=TRUE) ] )
}
