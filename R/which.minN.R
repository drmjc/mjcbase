## Get the indices of the N smallest values in x
##
## Parameters:
##     x: a vector of numbers
##     N: how many values are to be returned
##     return.ties: if there are multiple Nth smallest values, return all the indices? (T/F)
##
## Value:
##     return the indices of the N smallest values in x. possibly more than N if
##     return.ties=T and the Nth smallest value appears more than once.
##
## Mark Cowley, 24 May 2005
##
which.minN <- function(x, N=5, return.ties=F) {
    res <- (1:length(x))
    if( !is.null(names(x)) )
        names(res) <- names(x)

    res <- res[ order(x, decreasing=F, na.last=TRUE) ]

    if( return.ties ) {
        ## remove the 1:N min elements from x, then see if there are any more elements == the Nth element
        ## If there are, increment N so that they will all be returned.
        N <- N + sum(x[-res[1:N] ] == x[ res[N] ])
        return( res[1:N] )
    }
    else
        return( res[1:N] )
}


## Get the N smallest values from x
##
## Parameters:
##     x: a vector of numbers
##     N: how many values are to be returned
##     sort: if F: return the N values in order of appearance
##           if T: return the N values in increasing order.
##     return.ties: if there are multiple Nth smallest value, return them all? (T/F)
##
## Value:
##     return the N smallest values from x (possibly sorted), possibly more than N if
##     return.ties=T and the Nth smallest value appears more than once.
##
## Mark Cowley, 24 May 2005
##
minN <- function(x, N=5, sort=T, return.ties=F) {
    if( sort ) {
        x <- sort(x, decreasing=F, na.last=TRUE)
        if( return.ties )
            N <- N + sum(x[-c(1:N)] == x[ x[N] ], na.rm=TRUE)
        return(x[1:N])
    }
    else
        ## although counterintuitive, the sort call in the following sorts the INDICES which
        ## makes the returned values unsorted in reference to the values in x.
        return( x[ sort(which.minN(x, N, return.ties)) ] )
}


## Get the indices of the N largest values in x
##
## Parameters:
##     x: a vector of numbers
##     N: how many values are to be returned
##     return.ties: if there are multiple Nth largest values, return all the indices? (T/F)
##
## Value:
##     return the indices of the N largest values in x. possibly more than N if
##     return.ties=T and the Nth largest value appears more than once.
##
## Mark Cowley, 24 May 2005
##
which.maxN <- function(x, N=5, return.ties=F) {
    res <- (1:length(x))
    if( !is.null(names(x)) )
        names(res) <- names(x)

    res <- res[ order(x, decreasing=T, na.last=TRUE) ]

    if( return.ties )
        ## remove the 1:N min elements from x, then see if there are any more elements == the Nth element
        ## If there are, increment N so that they will all be returned.
        N <- N + sum(x[ -res[1:N] ] == x[ res[N] ], na.rm=T)

    return( res[1:N] )
}


## Get the N largest values from x
##
## Parameters:
##     x: a vector of numbers
##     N: how many values are to be returned
##     sort: if F: return the N values in order of appearance
##           if T: return the N values in decreasing order.
##     return.ties: if there are multiple Nth largest value, return them all? (T/F)
##
## Value:
##     return the N largest values from x (possibly sorted), possibly more than N if
##     return.ties=T and the Nth largest value appears more than once.
##
## Mark Cowley, 24 May 2005
##
maxN <- function(x, N=5, sort=T, return.ties=F) {
    if(sort) {
        x <- sort(x, decreasing=T, na.last=TRUE)
        if( return.ties )
            N <- N + sum(x[-c(1:N)] == x[ x[N] ], na.rm=T)
        return(x[1:N])
    }
    else
        ## although counterintuitive, the sort call in the following sorts the INDICES which
        ## makes the returned values unsorted in reference to the values in x.
        return( x[ sort(which.maxN(x, N, return.ties), na.rm=T) ] )
}
