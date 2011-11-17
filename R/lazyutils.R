#
# A bunch of lazy wrapper functions for general R utils
# like apply to rows/cols, col/row lengths etc...
#


nc <- function(x) { ncol(x) }
nr <- function(x) { nrow(x) }
l <- function(x) { length(x) }

rmall <- function() { rm(list=ls(), pos=1) }


uncsv <- function(x, trim=T) {
    res <- unlist(strsplit(x, ","))#[[1]]
    if( trim )
        res <- trim(res)

    return( res )
}

## function to strip off the text after (inclusive) the first match of "."
##
## eg: tsub("NM_008084.B") -> "NM_008084"
## eg: tsub("NM_008084.5") -> "NM_008084"
##
## Mark Cowley, 11 April 2006
##
tsub <- function(x) {
    sub("\\..*", "", x)
}

