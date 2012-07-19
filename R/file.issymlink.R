#' is the file a symbolic link?
#' 
#' @param f the path to file name(s)
#' @return a logical vector
#' @note This only works on unix-alikes, ie not windows.
#' 
#' @author Mark Cowley, 2012-07-16
#' @export
#' @examples
#' f1 <- tempfile()
#' writeLines(letters, f1)
#' f2 <- tempfile()
#' file.symlink(f1, f2)
#' file.issymlink(c(f1, f2))
#' # [1] FALSE  TRUE
#' unlink(c(f1, f2))
file.issymlink <- function(f) {
	res <- rep(FALSE, length(f))
	for(i in seq(along=f)) {
		cmd <- sprintf("file -h %s | grep symbolic > /dev/null; echo $?", shQuote(f[i]))
		res[i] <- system(cmd, intern=TRUE) == "0"
	}
	res
}
