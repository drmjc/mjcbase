#' Warning messages
#' 
#' Generates a warning message that corresponds to its argument(s)
#' and (optionally) the expression or function from which it was
#' called, which can be turned off by a commandline parameter
#'
#' @param expr an expression to evaluate
#' @param enable logical: if \code{TRUE} then suppressWarnings is enabled, if false, the \code{expr}
#'   is evaluated with no warning suppression
#' @return the result from evaluating \code{expr}
#' @author Mark Cowley, 2011-08-15
#' @export
suppressWarnings2 <- function(expr, enable=TRUE) {
	if( enable ) suppressWarnings(expr)
	else eval(expr)
}