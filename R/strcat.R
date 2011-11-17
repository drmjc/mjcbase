## concatenate a bunch of strings
##
## eg:
##     strcat("A", "A", "c")
##     [1] "AAc"
##
## Mark Cowley, 11 April 2006
##
strcat <- function(...) {
    paste(unlist( list(...) ), collapse="")
}
