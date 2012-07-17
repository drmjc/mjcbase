#' threshold a distance matrix
#' 
#' Code to take a distance matrix, and threshold out entitites that are too
#' distant from all other entities
#' 
#' @param d an object of class \code{dist}.
#' @param max.distance a single numeric distance threshold, eg \code{0.9}
#' @return an object of class \code{dist}, with rows/columns removed for entities that
#'   failed the thresholding.
#' @author Mark Cowley, 2009-09-03
#' @export
threshold.distance.matrix <- function(d, max.distance) {
	d2 <- as.matrix(d)
	diag(d2) <- NA
	idx <- union(
		which(rowMin(d2, na.rm=TRUE) > max.distance),
		which(colMin(d2, na.rm=TRUE) > max.distance) )
	d2 <- d2[-idx, -idx]
	diag(d2) <- 0
	d <- as.dist(d2)
}
