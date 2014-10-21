#' Debug a function, controlled by a boolean
#' 
#' The standard \code{\link[base]{debug}} function works great, but this function lets you turn
#' debuging on and off via a logical global parameter, meaning no more code
#' commenting
#' 
#' @param fun any interpreted R function. see \code{\link[base]{debug}}
#' @param text a text string that can be retrieved when the browser is entered.
#'   see \code{\link[base]{debug}}
#' @param condition a condition that can be retrieved when the browser is entered.
#'   see \code{\link[base]{debug}}
#' @param doit logical: if TRUE, then debugging mode will be entered, if \code{FALSE},
#'   no debugging will occur. Default=\code{TRUE} if running an interactive session.
#' @return nothing
#' @seealso \code{\link[base]{debug}}
#' @author Mark Cowley, 2011-08-12
#' @export
debug2 <- function(fun, text="", condition=NULL, doit=interactive()) {
	if( doit )
		debug(fun=fun, text=text, condition=condition)
}
