#' density triangles
#' 
#' Calculate the density of the values in the upper or lower triangle of x.
#' 
#' @inheritParams stats::density
#' @param \dots arguments passed to density
#' @author Mark Cowley, 4 May 2006
#' @export
#' @rdname density_tri
density_upper_tri <- function(x, ...) {
	density( x[upper.tri(x)], ...)
}

#' @export
#' @rdname density_tri
density_lower_tri <- function(x, ...) {
	density( x[lower.tri(x)], ... )
}
