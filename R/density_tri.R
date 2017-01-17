#' density triangles
#' 
#' Calculate the density of the values in the upper or lower triangle of x.
#' 
#' @param x the data from which the estimate is to be computed.
#' @param \dots arguments passed to density
#' @author Mark Cowley, 4 May 2006
#' @export
#' @importFrom stats density
#' @rdname density_tri
density_upper_tri <- function(x, ...) {
	density( x[upper.tri(x)], ...)
}

#' @export
#' @rdname density_tri
density_lower_tri <- function(x, ...) {
	density( x[lower.tri(x)], ... )
}
