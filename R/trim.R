#' Trim the whitespace from front and back of the words in a vector.
#'
#' @param x a character vector
#' @return a character vector of trimmed text
#' @author Mark Cowley, 2009-08-19
#' @examples
#' trim("   ABC")
#' # "ABC"
#' trim("DEF   ")
#' # "DEF"
#' trim("  ABC  ")
#' # "ABC"
#' @export
trim <- function(x) {
	x <- sub("^[ \t]+", "", x)
	x <- sub("[ \t]+$", "", x)
	x
}


test_trim <- function() {
	cat(dQuote(trim("  asdf")), "\n")
	cat(dQuote(trim("asdf   ")), "\n")
	cat(dQuote(trim("  asdf   ")), "\n")
	cat(dQuote(trim(c(NA, "  asdas  ", "  asdf", "asdf   ", "  asdf'(  ", "  ?asdf'(?  ", "  *asdf'(*$ "))), "\n")
}