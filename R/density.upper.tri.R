# Calculate the density of the values in the upper triangle
# of x.
#
# Mark Cowley, 4 May 2006
density.upper.tri <- function(x, ...) {
    density( x[upper.tri(x)], ...)
}



# Calculate the density of the values in the lover triangle
# of x.
#
# Mark Cowley, 4 May 2006
density.lower.tri <- function(x, ...) {
    density( x[lower.tri(x)], ... )
}
