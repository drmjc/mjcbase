#' Read a very wide table (ie many columns) into a data.frame
#' 
#' \code{\link{read.table}} admits that it's not so good at reading large tables with many
#' columns.
#' \code{\link{read.table}} recommends using \code{\link{scan}}, but \code{\link{scan}}
#'  is tricky unless you know the number of columns. 
#' NCOL is easy & VERY fast to determine on systems with \code{awk}.
#' This function essentially mashes the results from \code{\link{scan}} into a \code{\link{data.frame}}
#' On a 15MB file with 16x88742 elements, I got a 19x speed improvement using
#' this (25.5s) vs \code{\link{read.table}} (484.5s).
#' 
#' @details Common Error:
#' The message: \sQuote{Error in FUN(X[[1L]], ...) : the first argument must be of mode character} means
#' you should set \code{stringsAsFactors=FALSE}.
#' 
#' @param file the name of the file which the data are to be read from.
#'   Each row of the table appears as one line of the file.  If it
#'   does not contain an \emph{absolute} path, the file name is
#'   \emph{relative} to the current working directory, \code{\link{getwd}}().
#'   Tilde-expansion is performed where supported.  As from R
#'   2.10.0 this can be a compressed file (see \code{\link{file}}).
#' 
#'   Alternatively, \code{\link{file}} can be a readable text-mode connection
#'   (which will be opened for reading if necessary, and if so
#'   \code{\link{file}}d (and hence destroyed) at the end of the function
#'   call).  (If \sQuote{stdin()} is used, the prompts for lines may be
#'   somewhat confusing.  Terminate input with a blank line or an
#'   EOF signal, \sQuote{Ctrl-D} on Unix and \sQuote{Ctrl-Z} on Windows.  Any
#'   pushback on \sQuote{stdin()} will be cleared before return.)
#' 
#'   \code{\link{file}} can also be a complete URL.  (For the supported URL
#'   schemes, see the \sQuote{URLs} section of the help for \code{\link{url}}.)
#' 
#' @param header a logical value indicating whether the file contains the
#'   names of the variables as its first line.  If missing, the
#'   value is determined from the file format: \sQuote{header} is set to
#'   \sQuote{TRUE} if and only if the first row contains one fewer field
#'   than the number of columns.
#' 
#' @param sep the field separator character.  Values on each line of the
#'   file are separated by this character.  If \sQuote{sep = \dQuote{}} (the
#'   default for \code{\link{read.table}}) the separator is \sQuote{white space},
#'   that is one or more spaces, tabs, newlines or carriage
#'   returns.
#' 
#' @param quote the set of quoting characters. To disable quoting altogether,
#'   use \sQuote{quote = \dQuote{}}.  See \code{\link{scan}} for the behaviour on quotes
#'   embedded in quotes.  Quoting is only considered for columns
#'   read as character, which is all of them unless \sQuote{colClasses}
#'   is specified.
#' 
#' @param dec the character used in the file for decimal points.
#' 
#' @param row.names a vector of row names.  This can be a vector giving the
#'   actual row names, or a single number giving the column of the
#'   table which contains the row names, or character string
#'   giving the name of the table column containing the row names.
#' 
#'   If there is a header and the first row contains one fewer
#'   field than the number of columns, the first column in the
#'   input is used for the row names.  Otherwise if \sQuote{row.names} is
#'   missing, the rows are numbered.
#' 
#'   Using \sQuote{row.names = NULL} forces row numbering. Missing or
#'   \sQuote{NULL} \sQuote{row.names} generate row names that are considered to
#'   be \sQuote{automatic} (and not preserved by \code{\link{as.matrix}}).
#' 
#' @param col.names a vector of optional names for the variables.  The default
#'   is to use \sQuote{\dQuote{V}} followed by the column number.
#' 
#' @param as.is the default behavior of \code{\link{read.table}} is to convert character
#'   variables (which are not converted to logical, numeric or
#'   complex) to factors.  The variable \sQuote{as.is} controls the
#'   conversion of columns not otherwise specified by
#'   \sQuote{colClasses}.  Its value is either a vector of logicals
#'   (values are recycled if necessary), or a vector of numeric or
#'   character indices which specify which columns should not be
#'   converted to factors.
#' 
#'   Note: to suppress all conversions including those of numeric
#'   columns, set \sQuote{colClasses = \dQuote{character}}.
#' 
#'   Note that \sQuote{as.is} is specified per column (not per variable)
#'   and so includes the column of row names (if any) and any
#'   columns to be skipped.
#' 
#' @param na.strings a character vector of strings which are to be interpreted
#'   as \sQuote{NA} values.  Blank fields are also considered to be
#'   missing values in logical, integer, numeric and complex
#'   fields.
#' 
#' @param colClasses character.  A vector of classes to be assumed for the
#'   columns.  Recycled as necessary, or if the character vector
#'   is named, unspecified values are taken to be \sQuote{NA}.
#' 
#'   Possible values are \sQuote{NA} (the default, when \sQuote{type.convert} is
#'   used), \sQuote{\dQuote{NULL}} (when the column is skipped), one of the
#'   atomic vector classes (logical, integer, numeric, complex,
#'   character, raw), or \sQuote{\dQuote{factor}}, \sQuote{\dQuote{Date}} or \sQuote{\dQuote{POSIXct}}.
#'   Otherwise there needs to be an \sQuote{as} method (from package
#'   \sQuote{methods}) for conversion from \sQuote{\dQuote{character}} to the specified
#'   formal class.
#' 
#'   Note that \sQuote{colClasses} is specified per column (not per
#'   variable) and so includes the column of row names (if any).
#' 
#' @param nrows integer: the maximum number of rows to read in.  Negative and
#'   other invalid values are ignored.
#' 
#' @param skip integer: the number of lines of the data file to skip before
#'   beginning to read data.
#' 
#' @param check.names logical.  If \sQuote{TRUE} then the names of the variables in the
#'   data frame are checked to ensure that they are syntactically
#'   valid variable names.  If necessary they are adjusted (by
#'   \sQuote{make.names}) so that they are, and also to ensure that there
#'   are no duplicates.
#' 
#' @param fill logical. If \sQuote{TRUE} then in case the rows have unequal length,
#'   blank fields are implicitly added.  See \sQuote{Details}.
#' 
#' @param strip.white logical. Used only when \sQuote{sep} has been specified, and
#'   allows the stripping of leading and trailing white space from
#'   \code{\link{character}} fields (\code{\link{numeric}} fields are always stripped).
#'   See \code{\link{scan}} for further details (including the exact meaning
#'   of \sQuote{white space}), remembering that the columns may include
#'   the row names.
#' 
#' @param blank.lines.skip logical: if \sQuote{TRUE} blank lines in the input are
#'   ignored.
#' 
#' @param comment.char character: a character vector of length one containing a
#'   single character or an empty string.  Use \sQuote{\dQuote{}} to turn off
#'   the interpretation of comments altogether.
#' 
#' @param allowEscapes logical.  Should C-style escapes such as \sQuote{\\n} be
#'   processed or read verbatim (the default)?  Note that if not
#'   within quotes these could be interpreted as a delimiter (but
#'   not as a comment character).  For more details see \code{\link{scan}}.
#' 
#' @param flush logical: if \sQuote{TRUE}, \code{\link{scan}} will flush to the end of the line
#'   after reading the last of the fields requested.  This allows
#'   putting comments after the last field.
#' 
#' @param stringsAsFactors logical: should character vectors be converted to
#'   factors?  Note that this is overridden by \sQuote{as.is} and
#'   \sQuote{colClasses}, both of which allow finer control.
#' 
#' @param fileEncoding character string: if non-empty declares the encoding used
#'   on a file (not a connection) so the character data can be
#'   re-encoded.  See the \sQuote{Encoding} section of the help for
#'   \code{\link{file}}, the \sQuote{R Data Import/Export Manual} and \sQuote{Note}.
#' 
#' @param encoding encoding to be assumed for input strings.  It is used to mark
#'   character strings as known to be in Latin-1 or UTF-8 (see
#'   \sQuote{Encoding}): it is not used to re-encode the input, but
#'   allows R to handle encoded strings in their native encoding
#'   (if one of those two).  See \sQuote{Value}.
#' 
#' @param \dots Further arguments to be passed to \code{\link{read.table}}.
#' @return A \code{\link{data.frame}} containing a representation of the data in the file.
#' @rdname read.table.wide
#' @aliases read.table.wide
#' @author Mark Cowley, 2011-03-29
#' @export
#' @docType methods
#' 
read.table.wide <- function(
           file, header = FALSE, sep = "", quote = if(identical(sep, "\n")) "" else "\"",
           dec = ".", row.names, col.names,
           as.is = !stringsAsFactors,
           na.strings = "NA", colClasses = NA, nrows = -1,
           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           strip.white = FALSE, blank.lines.skip = TRUE,
           comment.char = "#",
           allowEscapes = FALSE, flush = FALSE,
           stringsAsFactors = default.stringsAsFactors(),
           fileEncoding = "", encoding = "unknown") {
	
	# nzchar(system("which awk", intern=TRUE)) || stop("awk not found on path")
	
	data <- scan(file, what="character", sep=sep, quote=quote, dec=dec, na.strings=na.strings, fill=fill, strip.white=strip.white, blank.lines.skip=blank.lines.skip, comment.char=comment.char, allowEscapes=allowEscapes, fileEncoding=fileEncoding, encoding=encoding, quiet=TRUE)
	# this is a long vector... need to convert it to a data.frame. but how many columns are there?
	
	# # how many columns are there in the file?
	# awk.sep <- ifelse(sep == "", "", sprintf("-F'%s'", sep))
	# cmd <- sprintf("cat %s | awk %s '{print NF}'", shQuote(file), awk.sep)
	# ncol <- as.numeric(system(cmd, intern=TRUE))
	# if( any(ncol != ncol[1]) ) stop("Irregular number of columns in each row. Can't handle this yet.")
	# ncol <- max(ncol)
	
	ncol <- length(scan(file, what="character", sep=sep, nlines=1, quiet=TRUE))
	if( length(data) %% ncol != 0 )
		stop("Number of data elements is not a multiple of the number of elements in first line")

	if( header ) {
		cn <- data[1:ncol]
		if( missing(col.names) ) {
			col.names <- cn
		}
		data <- data[(ncol+1):length(data)]
	}
	
	res <- as.data.frame(matrix(data, ncol=max(ncol), byrow=TRUE), stringsAsFactors=stringsAsFactors)
	if( !missing(col.names) ) {
		if( check.names ) {
			col.names <- make.names(col.names, unique = TRUE)
		}
		colnames(res) <- col.names
	}
	
	# res <- apply(res, 2, type.convert, na.strings = na.strings, as.is = as.is, dec = dec)
	res <- lapply(res, type.convert, na.strings = na.strings, as.is = as.is, dec = dec)
	res <- as.data.frame(res)

	if( !missing(row.names) ) {
		if( length(row.names) == 1 ) {
			if( is.character(row.names) ) {
				if(row.names %in% colnames(res))
					row.names <- match(row.names, colnames(res))
				else
					row.names <- 0
			}
			if( (row.names > 0) && (row.names < ncol(res)) ) {
				res <- column2rownames(res, row.names)
				res <- res[,-c(row.names)]
			}
		}
		else {
			if( length(row.names) == nrow(res) ) rownames(res) <- row.names
			else warning("length(row.names) != num rows")
		}
	}
	
	res
}
# CHANGELOG
# - 2013-09-10: added common error and the fix to the documentation.

#' @rdname read.table.wide
#' @aliases read.delim.wide
#' @export
read.delim.wide <- function(file, header = TRUE, sep = "\t", quote='"', dec=".", fill = TRUE, comment.char="", ...) {
	read.table.wide(file = file, header = header, sep = sep, quote = quote, 
	    dec = dec, fill = fill, comment.char = comment.char, ...)
}


#' @rdname read.table.wide
#' @aliases read.csv.wide
#' @export
read.csv.wide <- function(file, header = TRUE, sep = ",", quote='"', dec=".", fill = TRUE, comment.char="", ...) {
	read.table.wide(file = file, header = header, sep = sep, quote = quote, 
		dec = dec, fill = fill, comment.char = comment.char, ...)
}
