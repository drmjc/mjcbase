## Convert x into a data.frame with the data as characters.
##
## Mark Cowley, 25 Feb 2005
##
as.character.data.frame <- function(x) {
    x <- as.data.frame(x)
    for(i in 1:ncol(x))
        x[,i] <- as.character(x[,i])

    return(x)
}
