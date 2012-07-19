#' Paste the OSX clipboard into a character vector
#'
#' Enhanced control over object type\cr
#' The default is to read from clipboad into a character vector, and this will always
#' work.
#' You can attempt
#' to convert this to other objects, like \code{matrix} or \code{data.frame}, if
#' the clipboard contains suitable data (eg multi-line strings with tab's or comma's
#' separating fields).
#' We have tested this using vectors, 1 element per line; list, where each line
#' becomes its own list element, matrix and data.frame, where each line is a row,
#' each column is tab separated; also a Date class (see examples).
#' 
#' If you need finer control when importing to \code{matrix} or \code{data.frame}, we
#' recommend you use \code{read.table(..., file=pipe("pbpaste"), ...)}.
#' 
#' You can copy & paste actual representations of an R object complete with 
#' attributes & names etc using \code{dput}/\code{dget}. see examples.
#' 
#' @param class the class of the object to be created
#' @param sep the separator to use if \code{class="matrix"}, or \code{class="data.frame"}
#' @param ok logical: is it OK to be missing a final EOL?
#' @param warn logical: warn if the clipboard is missing a final EOL
#' 
#' @return a character vector of lines
#' 
#' @export
#' @author Mark Cowley, 2011-07-22
#' @examples
#' \dontrun{
#'   pbcopy(letters)
#'   input <- pbpaste()
#'   input <- pbpaste("character")
#' 
#'   pbcopy(1:10)
#'   pbpaste()
#'   pbpaste("numeric")
#' 
#'   df <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5)
#'   pbcopy(df)
#'   input <- pbpaste("data.frame")
#'   input <- pbpaste("matrix")
#'   input <- pbpaste("list")
#'   input <- pbpaste("Date") # this should fail
#' 
#'  pbcopy(Sys.Date())
#'  # structure(15286, class = "Date")
#'  pbpaste() # the class parameter is ignored, since the clipboard already knows its class
#'  pbpaste("character") # the class parameter is ignored, since the clipboard already knows its class
#' 
#' # here's a way to copy the actual object represntation to & from the clipboad 
#' # (great for copying objects between R sessions)
#' dput(letters, pipe("pbpaste", "w"))
#' dget(pipe("pbcopy"))
#' 
#' }
#' @seealso \code{\link{readLines}}, \code{\link{pbcopy}}, the \dQuote{Clipboard} section within ?\code{\link{file}}
#' @rdname pbpaste
#' @aliases pbpaste
pbpaste <- function(class="character", sep="\t", ok=TRUE, warn=TRUE) {
	isMac() || stop("This only works on Mac OSX")
	
	OUT <- pipe("pbpaste")
	res <- readLines(OUT)
	# There's a dirty R bug where if the final line does not have an EOL,
	# then it is silently skipped by readLines(OUT).
	# If there's at least 2 lines, then there's a dirty hack to fix this, but 
	# if there's only a single line, then I can't get any variations of
	# readLines(), flush(), readLines() to work...
	if( isIncomplete(OUT) ) {
		if( !ok ) stop("Final line missing trailing EOL")
		if( length(res) == 0 ) stop("Due to an R bug, this function doesn't work when selecting a single line of text with no trailing EOL")
		if( warn ) warning("Final line missing trailing EOL")

		flush(OUT)
		res2 <- readLines(OUT)
		# the last element is pre-pended to the first element; ie A, B, C, Dog -> DogA, B, C;
		firstelem <- res[1]
		lastelem <- res2[1]
		lastelem <- substring(lastelem, 1, nchar(lastelem)-nchar(firstelem))
		res <- c(res, lastelem)
	}
	close(OUT)
	# was this an object written via dput?
	if( grepl("^structure\\(", res[1]) ) {
		OUT <- pipe("pbpaste")
		res <- dget(OUT)
		close(OUT)
	}
	else {
		as.df <- function(x, sep="\t") {
			res <- strsplit(x, sep)
			res <- t(as.data.frame(res))
			res <- as.data.frame(res) # if data is all numeric, res becomes a matrix.
			row.names(res) <- NULL
			res
		}
		res <- switch(class,
			character=res,
			data.frame=as.df(res, sep=sep),
			matrix=as.matrix(as.df(res, sep=sep)),
			as(res, class)
		)
	}
	
	res
}
# CHANGELOG
# 2012-03-14:
# - fixed silent bug when there was a trailing newline character.

#' @rdname pbpaste
#' @aliases pbpaste
#' @export
paste.from.clipboard <- function(class="character", sep="\t") pbpaste(class, sep)


#' Write a vector to the OSX clipboard
#'
#' The input, \code{x} will be written to the clipboad, using one of
#' these functions:
#' \tabular{ll}{
#'   \code{vector} \tab \code{writeLines} \cr
#'   \code{list} \tab \code{writeLines.list} \cr
#'   \code{matrix} \tab \code{write.delim} \cr
#'   \code{data.frame} \tab \code{write.delim} \cr
#'   other \tab \code{dput}
#' }
#' @param x an object to be writen to the clipboard. see Details.
#' @return nothing
#' @export
#' @author Mark Cowley, 2011-11-08
#' @examples
#' \dontrun{
#' pbcopy(letters)
#' pbcopy(matrix(letters[1:25], 5))
#' pbcopy(data.frame(letters,LETTERS,1:26))
#' pbcopy(list(a=letters, b=LETTERS, c=as.character(1:26)))
#' pbcopy(Sys.Date())
#' }
#' @seealso \code{\link{pbcopy}}, the \dQuote{Clipboard} section within ?\code{\link{file}}
#' @rdname pbcopy
#' @aliases pbcopy
pbcopy <- function(x) {
	isMac() || stop("This only works on Mac OSX")
	
	OUT <- pipe("pbcopy", "w")
	if( is.list(x) )
		writeLines.list(x, OUT)
	else if( is.vector(x) && !is.list(x) )
		writeLines(as.character(x), OUT)
	else if( is.matrix(x) || is.data.frame(x) )
		write.delim(x, OUT, row.names=!is.null(rownames(x)))
	else
		dput(x, OUT)

	close(OUT)
}

#' @rdname pbcopy
#' @aliases copy.to.clipboard
#' @export
copy.to.clipboard <- function(x) pbcopy(x)
