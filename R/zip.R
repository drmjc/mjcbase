#' Create a zip archive.
#' 
#' This function creates zip archives, either of single files, multiple files,
#' a single directory, multiple directories, or combinations of files and
#' directories. Currently, \code{recurse=TRUE}, which you'll only want to disable if
#' you really don't want directories to be recursed into (ie it's ok to leave
#' \code{recurse=TRUE} if you only have a bunch of files to compress.)
#' 
#' @param x a vector of file or dir names.
#' @param zip.file the name (including the path) of the zip file to be created.
#'   Should end in .zip or .ZIP
#' @param recurse logical: recurse into directories? (-r)
#' @param store logical: store only? (-0), i.e., do not compress.
#' @param quiet logical: quiet operation (-q)
#' 
#' @return none. a zip file archive is made.
#' 
#' @section Warning:
#' If you specify a vector
#'   of filenames, if they are not all in the same directory, then the
#'   resulting archive preserves those different directory paths. i.e., when
#'   you unzip the archive, you will get a minimal representation of the
#'   filesystem that captures the different paths of the files. You should copy
#'   files into the same directory, then zip them if you do not want this
#'   behaviour! NB this copies the default behaviour of using zip on the
#'   command line. Tested on OSX using Zip 2.32 from the Info-ZIP team, which
#'   exists at /usr/bin/zip both on OSX (10.5), and solaris 10.
#' 
#' @section Todo:
#'  just like \code{options("unzip")}, implement an \code{options("zip")}
#' 
#' @author Mark Cowley, 2009-10-15
#' @export
zip <- function(x, zip.file, recurse=TRUE, store=FALSE, quiet=TRUE) {
	zip.cmd <- "/usr/bin/zip"
	# todo: use options()$zip

	x <- shQuote(x)
	x <- paste(x, collapse=" ")
	args <- paste(
		ifelse(recurse, "-r", ""),
		ifelse(store, "-0", ""),
		ifelse(quiet, "-q", "")
		)
	cmd <- paste(zip.cmd, args, shQuote(zip.file), x)

	system(cmd, intern=FALSE)
}
