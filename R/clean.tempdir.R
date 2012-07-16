#' Function to delete all of the files in the tempdir.
#' 
#' @author Mark Cowley, 5 April 2006
#' @export
clean.tempdir <- function() {
	files <- dir(path=tempdir(), full.names=TRUE)
	tmp <- file.remove(files)

	if(length(tmp) == 0 || all(tmp))
		cat("tmpdir is now empty\n")

}
