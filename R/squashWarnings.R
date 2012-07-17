#' Catch any warnings & convert them to messages
#' 
#' Evaluate an expression which may generate warnings. Instead of printing the 
#' often terse warning to stderr, instead catch the warnings, print them to 
#' stdout using the message mechanism.
#'
#' @param expr an expression that might generate a warning
#' @return The result of executing expression. 
#' One message is returned per warning, with each message having the form 
#' \sQuote{WARNING: w\$message}
#' @author Mark Cowley
#' @examples
#' # no warning or error
#' squashWarnings(1:5)
#' # warning thrown
#' squashWarnings(warning("My warning message"))
#' # errors are still thrown:
#' squashWarnings(log("x"))
#' @export
squashWarnings <- function(expr) {
	w.list <- NULL
	w.handler <- function(w) { # warning handler
		w.list <<- c(w.list, w$message) # save warning
		invokeRestart("muffleWarning")
	}
	res <- withCallingHandlers(
		try(eval(expr)), 
		warning=w.handler)
	if( !is.null(w.list) ) {
		for(i in 1:length(w.list)) {
			message(paste("WARNING:",w.list[i]))
		}
	}
	return( res )
}


# #' Catch any warnings and errors & convert them to messages
# #' 
# #' Evaluate an expression which may generate warnings, and/or errors.
# #' Instead of printing the often terse error or warning to stderr, 
# #' instead catch the errors and warnings, print them to stdout using 
# #' the message mechanism.
# #
# #' @param expr an expression that might generate a warning or error
# #' @return The result of executing expression. 
# #' One message is returned per error, with each message having the form 
# #' \sQuote{ERROR: w\$message}
# #' One message is returned per warning, with each message having the form 
# #' \sQuote{WARNING: w\$message}
# #' @author Mark Cowley
# #' @examples
# #' squashWarningsAndErrors(1:5)
# #' squashWarningsAndErrors(simpleWarning("My warning message"))
# #' squashWarningsAndErrors(log("x"))
# squashWarningsAndErrors <- function(expr) {
# 	w.list <- NULL
# 	w.handler <- function(w) { # warning handler
# 		 w.list <<- c(w.list, w$message) # save warning
# 		 invokeRestart("muffleWarning")
# 	}
# 	e.list <- NULL
# 	   e.handler <- function(e){ # error handler
# 		 err <- c(e.list, e$message, e$call) # save error
# 		 return( err )
# 	}
# 	res <- withCallingHandlers(
# 				   try(eval(expr)),
# 				   warning=w.handler,
# 				   error=e.handler)
# 	if( !is.null(e.list) ) {
# 		for(i in 1:length(e.list)) {
# 			message(paste("ERROR:",e.list[i]))
# 		}
# 	}
# 	if( !is.null(w.list) ) {
# 		for(i in 1:length(w.list)) {
# 			message(paste("WARNING:",w.list[i]))
# 		}
# 	}
# 	res
# }
