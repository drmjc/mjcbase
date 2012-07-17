#' isWindows
#'
#' @return \code{TRUE} if current platform is Windows, \code{FALSE} otherwise
#' @author Mark Cowley, 2011-10-21
#' @export
#' @examples
#' isWindows()
isWindows <- function() {
	Sys.info()[["sysname"]]=="Windows"
}

#' isMac
#'
#' @return \code{TRUE} if current platform is Mac, \code{FALSE} otherwise
#' @author Mark Cowley, 2011-10-21
#' @export
#' @examples
#' isMac()
isMac <- function() {
	Sys.info()[["sysname"]]=="Darwin" 
}
