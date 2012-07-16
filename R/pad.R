#' Pad a number with leading zeros
#' 
#' Convert a number to a string, and pad out the resulting character with "0" at the start.
#'
#' Can supply \code{strWidth} to specify the number of characters desired in the final string. If it's
#' not supplied, then a single "0" is prepended. If \code{int} is
#' a vector of numbers, \code{strWidth} is automatically set so that they all have the same number of
#' characters.
#' @param int a numeric vector, or character vector of numbers
#' @param strWidth the desired width. If \code{NULL}, and \code{length(int)>1}, 
#' then this is determined automatically. If \code{NULL}, and \code{length(int)==1}, 
#' then a single \dQuote{0} is added.
#' @param atEnd logical: pad at the end (\code{TRUE}), or start (\code{FALSE})?
#' 
#' @return a charcter vector, all with the same \code{nchar}.
#' @author Mark Cowley, 2005-09-21
#' @seealso \code{\link{sprintf}}
#' @export
#' @examples
#' pad(1, 2)
#' pad(1:10, 2)
#' pad(1:10)
#'
pad <- function(int, strWidth=NULL, atEnd=FALSE) {
	# if(!atEnd && all(is.numeric(int)) && !all(is.integer(int)))
	#	  atEnd <- TRUE

	MAX <- max(nchar(int))
	if( is.null(strWidth) ) {
		if( length(int) > 0 ) {
			strWidth <- MAX
		}
		else
			strWidth <- nchar(int) + 1
	}
	else if (strWidth < MAX) {
		stop(sprintf("strWidth not wide enough: suggest %d", MAX))
	}


	for(i in 1:length(int)) {
		if( !atEnd )
			int[i] <- paste(c(rep("0", strWidth - nchar(int[i])), int[i]), collapse="", sep="")
		else
			int[i] <- paste(c(int[i], rep("0", strWidth - nchar(int[i]))), collapse="", sep="")
	}

	return(int)
}
