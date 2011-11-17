## are the elements in x all unique?
##
## Mark Cowley, 6 April 2006
##
is.unique <- function(x) {

    return( length(x) == length(unique(x)) )

}