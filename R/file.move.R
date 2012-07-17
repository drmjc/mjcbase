#' move files into a directory
#' 
#' Even if the files in \code{files} are from different paths, they 
#' will all end up in the same \code{dir}, ie the sub-paths that differ
#' will not be preserved.
#' 
#' @param files character vector, containing file name(s) or path(s).
#' @param dir character(1) the path to a directory
#' @return logical vector
#' @author Mark Cowley, 2011-11-09
#' @export
file.move <- function(files, dir) {
	!missing(files) && all(is.file(files)) || stop("files must be a vector of valid file paths")
	!missing(dir) && length(dir) == 1 && is.dir(dir) || stop("dir must be a valid path to a directory.")
	all(is.file(files)) || stop("Currently only supports moving files.")
	to <- file.path(dir, basename(files))
	file.rename(files, to)
}
