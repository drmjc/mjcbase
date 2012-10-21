#' Concatenate strings
#' 
#' @description Concatenate vectors after converting to character.
#' @details R 2.15 (maybe R 2.14) introduced the very useful paste0. 
#' This allows some of my legacy code to use paste0 in R < 2.15
#'
#' @param \dots one or more R objects, to be converted to character vectors.
#' @param collapse an optional character string to separate the results. Not \code{NA}.
#' @return A character vector of the concatenated values.  This will be of
#'     length zero if all the objects are, unless \code{collapse} is non-NULL
#'     in which case it is a single empty string.
#' @author Mark Cowley, 2012-10-19
#' @export
paste0 <- function(..., collapse = NULL) {
	if( as.numeric(R.Version()$minor) < 15.0 ) paste(..., sep="", collapse=collapse)
	else base::paste0(..., collapse=collapse)
}
