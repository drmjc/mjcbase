#' Write a matrix-like object as a tab-delimited txt file.
#' 
#' This method writes tab delimited files and offers enhanced control over the
#' writing of a row.names column.
#' 
#' Writing row names.\cr
#' Allowable values for \code{row.names} are:
#' \tabular{ll}{
#'   \code{TRUE} \tab row names are written with a blank column header \cr
#'   \code{FALSE} \tab no row names are written \cr
#'   \code{NULL} or \code{NA} \tab row names are written, with no column header, 
#'                                 just like \code{\link{write.table}} \cr
#'   the name of a column \tab row names will be written, with the column header 
#'                             set to the value of \code{row.names}
#' }
#' Note that If \code{x} has no row and/or column names, then no row and/or column names \cr
#' will be written, no matter what the settings for \code{row.names} or \code{col.names}
#' are.\cr
#'
#' If you use the default options, then it works nicely with
#' \code{\link{read.delim}}.
#' 
#' @param x a \code{matrix} or \code{data.frame}
#' @param file The file name (including path) which will be created. It will be
#'   over-written without warning.
#' @param na What to replace \code{NA}'s with. Sensible options include \dQuote{---}
#'   (the default), \dQuote{}, or \dQuote{-}.
#' @param row.names either \code{TRUE}, \code{FALSE}, \code{NULL}, \code{NA}, 
#'   or a \code{character(1)}.  See Details.
#' @param col.names logical: If \code{TRUE} then write out the column names.
#' @param append logical: if \code{TRUE}, then append to end, else overwrite the file.
#' @param \dots Further arguments passed to \code{\link{write.table}}
#' @return a tab-delimited txt file is written out.
#' @author Mark Cowley
#' @seealso \code{\link{write.table}}, \code{\link{read.delim}}
#' @keywords IO file
#' @importFrom utils write.table
#' @export
#' 
write.delim <- function(x, file, na="---",
						row.names = FALSE, col.names = TRUE, 
						append = FALSE, ...) {
	!missing(x) && (is.matrix(x) || is.data.frame(x)) || stop("x must be a matrix, or data.frame")
	!missing(file) || stop("file must be set")

	if( is.null(rownames(x)) ) {
		row.names <- FALSE
	}
	else if( is.null(row.names) || is.na(row.names) ) {
		# this is the default method of write.table -- ie 1 fewer column name than there are columns
		row.names <- TRUE
	}
	else if( is.logical(row.names) && row.names ) {
		x <- data.frame(rownames(x), x, check.names=FALSE)
		colnames(x)[1] <- ""
		row.names <- FALSE
	}
	else if ( is.logical(row.names) && !row.names ) {
		# do nothing
		row.names <- FALSE
	}
	else if( is.character(row.names) && length(row.names) == 1 ) {
		x <- data.frame(rownames(x), x, check.names=FALSE)
		colnames(x)[1] <- row.names
		row.names <- FALSE
	}
	else {
		stop('Invalid row.names argument: NULL|NA|FALSE|TRUE|"MyColumnNameHeader"\n')
	}
	
	if( is.null(colnames(x)) ) col.names <- FALSE

	# if first 2 letters are ID then Excel Mac 2004 thinks the file is a SLYK file and fails.
	if( (is.matrix(x) || is.data.frame(x)) && col.names && colnames(x)[1] == "ID" )
		colnames(x)[1] <- ".ID"

	write.table(x, file, 
		na=na, row.names=row.names, col.names=col.names, append=append, 
		quote=FALSE, sep="\t", eol="\n", dec=".", qmethod=c("escape", "double"), ...)
}
# CHANGELOG
# 12/10/04: v1
# 2009-01-28: heavily edited
# 2010-11-24: bug fix allows vectors to be written
# 2011-11-08: 
# - rox updates
# - cbind->data.frame
# - added is.null(col.names(x)) and is.null(row.names(x)) checks
# 2012-03-12:
# - dropped the is.matrix.like
# 2014-10-21:
# - added check.names=FALSE to avoid numeric column names from being mutated.
