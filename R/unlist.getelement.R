## Curious function to unlist a list, and if each list element happens to be a vector
## of values, only get the values indexed my element:
## eg: x <- list(c(1,2,3), c('a', 'b', 'c'))
##     unlist.getelement(x, 1)
##         "1" "a"
##
## initially written to extract info from mgu74av2GENENAME environment which can often have
## multiple GENENAME's per probeset and i want to convert this into a column of a table by
## getting only the first element
##
## Mark Cowley, 25 Feb 2005
unlist.getelement <- function(data, element=0) {
    vec <- character(length=length(data))
    for(i in 1:length(data))
        vec[i] <- data[[i]][1]
    names(vec) <- names(data)

    return(vec)
}
