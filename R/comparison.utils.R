## Functions to simplify Less Than, Greater than etc -
## avoids typing long object names twice ;)
## very lazy
##
## Mark Cowley, 12 Jan 2005
##
lt <- function(data, val) {
    return(data[data < val])
}

gt <- function(data, val) {
    return(data[data > val])
}

lte <- function(data, val) {
    return(data[data <= val])
}

gte <- function(data, val) {
    return(data[data >= val])
}

eq <- function(data, val) {
    return(data[data == val])
}

## Are the values in numeric vectors x and y the same (give-or-take threshold)
##
## Mark Cowley, 4 Feb 2005
##
same <- function(x, y, threshold=0.0001) {
    abs(x-y) < threshold
}
