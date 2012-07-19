#' set the width of the current display
#' set the width of the current display to the number of columns in the current
#' terminal session
#' @param width integer: the number of columns to set the width to. if \code{NULL}, then this
#' is determined via \code{Sys.getenv("COLUMNS")}
#' @author Mark Cowley, 21 Feb 2007
#' @export
setwidth <- function(width=NULL) {
	if( is.null(width) ) {
		COLUMNS <- Sys.getenv("COLUMNS")
		width <- as.numeric( COLUMNS )
	}
	if(!is.na(width) && is.numeric(width) && width > 0) {
		options(width=width)
	}
}
