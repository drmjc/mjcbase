#' find matching package names using grep
#'
#' There's some R packages that I just can't for the life of me remember
#' the correct caPitAlisaTion. This function searches the list of currently
#' installed packages for ones matching your pattern, and returns the
#' names of matching packages. 'pkgrep' is short for 'package grep'
#' 
#' @param pattern a search pattern
#' @param \dots arguments passed to \code{\link{grep}}
#' 
#' @return a character vector of matching packages
#' 
#' @author Mark Cowley, 2012-03-27
#' @export
#' @importFrom utils installed.packages
#' @examples
#' pkgrep("lumi")
#' # [1] "illuminaHumanv4.db" "lumi"               "lumidat"           
#' # [4] "methylumi"
#' 
pkgrep <- function(pattern, ...) {
	grep(pattern, rownames(installed.packages()), value=TRUE, ...)
}
