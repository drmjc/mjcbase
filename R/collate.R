#' collate data together
#' 
#' collate data, that is, interleave x1,y1,x2,y2,...,xN,yN.
#' x and y must have same dimensions
#' 
#' @details collate.data.frame: Collate the columns from two \code{data.frame}'s. ie produce a table that alternates
#' columns from the 2 tables. They must be identical dimensions.
#' @param x a vector or 2D object
#' @param y a vector or 2D object, identically sized to x
#' @return the same data type as x, with columns or data interleaved.
#' @author Mark Cowley
#' 
#' @export
#' @S3method collate default
#' @S3method collate numeric
#' @S3method collate character
#' @S3method collate data.frame
#' 
#' @examples
#' # character
#' collate(letters, LETTERS)
#' 
#' collate(1:10, 11:20)
#' 
#' # data.frame
#' a <- iris[1:5,]; colnames(a) <- letters[1:5]
#' b <- iris[1:5,]; colnames(b) <- LETTERS[1:5]
#' collate(a, b)
collate <- function(x,y) UseMethod("collate")
# CHANGELOG
# 2009-12-16 - original created, called collate.data.frame
# 2013-08-09 - made this into S3 generic & added numeric, character.

collate.default <- function(x,y) {
	res <- collate(as.character(x), as.character(y))
	res <- as(res, class(x))
}

collate.data.frame <- function(x,y) {
	identical(dim(x), dim(y))
	
	res <- data.frame(x,y)
	o <- collate(1:ncol(x), 1:ncol(y)+ncol(x))
	res <- res[,o]
	res
}

collate.numeric <- function(x,y) {
	stopifnot(length(x)==length(y))
	res <- c(x,y)
	o <- c(seq(1, length(res), 2), seq(2,length(res),2))
	o <- order(o)
	res <- res[o]
	res
}

collate.character <- function(x,y) {
	stopifnot(length(x)==length(y))
	c(x,y)[collate(1:length(x), 1:length(y)+length(x))]
}
