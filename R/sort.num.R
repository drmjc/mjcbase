
## sort a vector of words if those words all contain a different numerical identifier,
## eg string1, string2, string3.
## The defaul alpha-numeric sort would return:
## string12, string10, string100, string101... which is often not the desired sort
##
## Mark Cowley, 3 May 2005
##
sort.num <- function(x, decreasing=F) {
    x2 <- as.numeric(gsub("[^0-9]", "", x))
    if(all(!is.na(x2)))
        x <- x[order(x2, decreasing=decreasing)]
    else {
        isnum <- which(!is.na(x2))
        if( length(isnum) > 0 )
            x <- c(sort.num(x[isnum], decreasing=F), sort(x[-isnum], decreasing=F))
        else
            x <- sort(x, decreasing=F)

        if( decreasing )
            x <- rev( x )
    }

    return( x )
}

## Will get all the files that are in dir and satisfy pattern, and will then
## sort them according to any numbers which are in the file names.
## suitable for importing lots of files that are named via an increasing numerical
## index eg tmp1.out, tmp2.out, tmp3.out etc.
## This function exists because the default of dir() is to return the files as:
## tmp1.out, tmp10.out, tmp100.out, tmp101.out etc...
##
## Value:
##     files sorted via a monotonically increasing id
##
## Mark Cowley, 3 May 2005
##
dir.sort <- function(path=".", pattern=NULL, ...) {
    files <- dir(path, pattern, ...)

    files <- sort.num(files)

    return( files )
}
