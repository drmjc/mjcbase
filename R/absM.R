# absolute value a matrix whilst retaining its dimensions.
#
# Mark Cowley, 22 Feb 2006
absM <- function(x) {
    return( matrix(abs(x), nrow=nrow(x)) )
}
