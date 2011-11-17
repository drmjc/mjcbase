## Function to check ifa given element exists within a list.
## eg has.element(genomescan, "summary") checks if
##  genomescan$summary exists.
##
## Mark Cowley, 21 Jan 2005
##
has.element <- function(x, elem) { 
    return(length(grep(as.character(elem), names(x))) > 0)
}
