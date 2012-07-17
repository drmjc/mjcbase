#' A printed representation of a 2D venn diagram
#' 
#' \code{~} means NOT so A~B means in A, but not in B\cr
#' \code{&} means AND, i.e. intersection\cr
#' \code{|} means OR, i.e. union\cr
#' 
#' @param x a vector
#' @param y a vector
#' @return none. prints to stdout
#' @author Mark Cowley, 19 April 2006
#' @examples
#' print.venn(1:10, 5:20)
#' #   A ~ B A & B B ~ A A | B
#' # N     4     6    10    20
#' # %    20    30    50   100
#' @export
print.venn <- function(x, y) {
    all <- intersect(x, y)
    x <- setdiff(x, all)
    y <- setdiff(y, all)
    tmp <- c(
        length(x),
        length(all),
        length(y) )

    res <- matrix(c(tmp, sum(tmp)), nrow=1)
    res <- rbind(as.character(res), round( res[1,] / res[1,4] * 100, 2))
    colnames(res) <- c("  A ~ B", "  A & B", "  B ~ A", "  A | B")
    rownames(res) <- c(" N", " %")
    res <- as.data.frame(res)

    res
}


#' Write a 2D Venn diagram to an Excel file.
#' Compare 2 vectors, and export an XLS file listing 4 comparisons: union,
#' intersection, only in x, or y
#' Optionally, the 2 input vectors can be
#' exported via the \code{exportXY=TRUE} parameter.
#' 
#' @param x a vector
#' @param y a vector
#' @param file the xls file to write to
#' @param names a character[2] corresponding to the names for \code{x} and \code{y}
#' @param sortfun the function to sort the elements
#' @param exportXY logical: if TRUE, then the original x, y values are written in first 2 columns
#' @return a 4 column XLS file is written, with these names: \dQuote{only in x}, 
#'   \dQuote{only in y}, \dQuote{intersect}, \dQuote{union}
#' @examples
#' f <- tempfile()
#' write.venn.2D.xls(LETTERS[1:10], LETTERS[7:15], f)
#' @author Mark Cowley, 2009-02-02
#' @export
write.venn.2D.xls <- function(x, y, file, names=c("A", "B"), sortfun=sort, exportXY=FALSE) {
	length(names) == 2 || stop("names must have length=2")
	
	X <- sortfun(x); Y <- sortfun(y)
	both   <- sortfun( intersect(x, y) )
	either <- sortfun( union(x, y) )
	x      <- sortfun( setdiff(x, both) )
	y      <- sortfun( setdiff(y, both) )

	# pad with NA's
	.pad <- function(a, len) {
		c(a, rep(NA, len-length(a)))
	}
	N <- length(either)
	both <- .pad(both, N)
	x    <- .pad(x,    N)
	y    <- .pad(y,    N)

	res <- cbind(x, y, both, either)
	colnames(res) <- c(paste("only in", names[1]), paste("only in", names[2]), "intersection", "union")
	if( exportXY ) {
		res <- cbind(x=.pad(X,N), y=.pad(Y,N), res)
		colnames(res)[1:2] <- names
	}

	write.xls(res, file, na="")
}
# CHANGELOG
# 2011-09-01:
# - added exportXY parameter
# - bug fix where union did not include those in the intersection


#' Write a 3D Venn diagram to an Excel file.
#' Compare 3 vectors, and export an XLS file listing 9 comparisons:\cr
#' \tabular{l}{
#' 	union \cr
#' 	intersection \cr
#' 	only in x \cr
#' 	only in y \cr
#' 	only in z. \cr
#' 	only in x and y \cr
#' 	only in x and z \cr
#' 	only in y and z \cr
#' 	only in 2 of the 3 comparisons \cr
#' }
#' 
#' Optionally, the 3 input vectors can be
#' exported via the \code{exportXYZ=TRUE} parameter.
#' 
#' @param x a vector
#' @param y a vector
#' @param z a vector
#' @param file the xls file to write to
#' @param names a character[3] corresponding to the names for \code{x}, \code{y} and \code{z}
#' @param sortfun the function to sort the elements
#' @param exportXYZ logical: if TRUE, then the original x, y, z values are written in first 3 columns
#' @return a 9 or 12 column XLS file is written, with these names:
#' \item{x}{All values in x, if exportXYZ=TRUE}
#' \item{y}{All values in x, if exportXYZ=TRUE}
#' \item{z}{All values in x, if exportXYZ=TRUE}
#' \item{only in x}{Those values only found in x}
#' \item{only in y}{Those values only found in y}
#' \item{only in z}{Those values only found in z}
#' \item{only in x and y}{Those values only found in x and y (ie not in the intersection of all 3)}
#' \item{only in x and z}{Those values only found in x and y (ie not in the intersection of all 3)}
#' \item{only in y and z}{Those values only found in x and y (ie not in the intersection of all 3)}
#' \item{only in any 2}{Those values only found in any of the pairwise comparisons, and not in intersection of all 3}
#' \item{intersect}{Those values in all 3}
#' \item{union}{Those values in any gene list}
#' @examples
#' f <- tempfile()
#' write.venn.3D.xls(LETTERS[1:10], LETTERS[7:15], LETTERS[9:20], f)
#' @author Mark Cowley, 2009-02-02
#' @export
write.venn.3D.xls <- function(x, y, z, file, names=c("A", "B", "C"), sortfun=sort, exportXYZ=FALSE) {
	!missing(x) && !missing(y) && !missing(z) || stop("must supply the x,y,z vectors")
	length(names) == 3 || stop("names must have length=3")
	
	names <- shQuote(names)
	
	X <- sortfun(x<-unique(x)); Y <- sortfun(y<-unique(y)); Z <- sortfun(z<-unique(z))
	
	all    <- sortfun( intersectN(X, Y, Z) )
	either <- sortfun( unionN(X, Y, Z) )
	x      <- sortfun( setdiff(x, union(Y,Z)) )
	y      <- sortfun( setdiff(y, union(X,Z)) )
	z      <- sortfun( setdiff(z, union(X,Y)) )
	xy     <- sortfun( setdiff(intersect(X,Y), all) )
	xz     <- sortfun( setdiff(intersect(X,Z), all) )
	yz     <- sortfun( setdiff(intersect(Y,Z), all) )
	any2   <- sortfun( union(xy, union(xz, yz)) )
	
	# pad with NA's to make cbind work
	.pad <- function(a, len) {
		c(a, rep(NA, len-length(a)))
	}
	N <- length(either)
	all  <- .pad(all,  N)
	x    <- .pad(x,    N)
	y    <- .pad(y,    N)
	z    <- .pad(z,    N)
	xy   <- .pad(xy,   N)
	xz   <- .pad(xz,   N)
	yz   <- .pad(yz,   N)
	any2 <- .pad(any2, N)

	res <- cbind(x, y, z, xy, xz, yz, any2, all, either)
	colnames(res) <- c(
		paste("only in", names[1]), 
		paste("only in", names[2]), 
		paste("only in", names[3]), 
		sprintf("only in %s and %s", names[1], names[2]),
		sprintf("only in %s and %s", names[1], names[3]),
		sprintf("only in %s and %s", names[2], names[3]),
		"only in any 2",
		"intersect", 
		"union"		
	)
	
	if( exportXYZ ) {
		res <- cbind(x=.pad(X,N), y=.pad(Y,N), z=.pad(Z,N), res)
		colnames(res)[1:3] <- names
	}
	
	write.xls(res, file, na="")
}
# CHANGELOG:
# 2011-09-01
# - added exportXYZ parameter
# - bug fix where union did not include those in the intersection
# 2011-11-02
# - added 3 columns for: in x & y ! u(x,y,z) columns
# - added any2
# - bug fix in exportXYZ .pad(..., N)
# 2012-02-23:
# - bug fix in only in 'x'; only in 'y'; only in 'z'