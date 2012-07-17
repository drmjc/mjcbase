#' Unzip a zip archive file
#' Unzip a zip archive file, crossplatform - ie on mac, windows and linux
#' If \code{zip.filename} is a URL, it is first downlaoded to \code{dest}
#' On MacOS, this function removes the hidden ._ files, and __MACOSX folder
#' which get created by the Finder>Create Archive program on some Mac platforms.
#' As with all zip files, if the zipped files are not within a subdirectory, 
#' they will all be extracted to \code{destdir}, so you may want to ensure
#' that the \code{destdir} is clean and empty
#' 
#' @param zip.filename the path, or url to a single zip file
#' @param dest the path to the destination directory. If \code{NULL}, then
#'  it is unzipped in the current working directory
#' @param verbose logical: noisy?
#' @return a character vector of paths to the files that were unzipped.
#' @author Mark Cowley, 2010-01-08
#' @export
#' @examples
#' \dontrun{
#' # f <- "http://mirror.aarnet.edu.au/pub/CRAN/bin/windows/contrib/r-release/sos_1.3-1.zip"
#' f <- "http://cran.r-project.org/bin/windows/contrib/r-release/sos_1.3-1.zip"
#' # cran.mirror <- getOption("repos", default="http://cran.r-project.org")
#' # f <- paste(cran.mirror, "bin/windows/contrib/r-release/sos_1.3-1.zip", sep="/")
#' files <- crossplatform.unzip(f, "/tmp")
#' files
#' }
crossplatform.unzip <- function(zip.filename, dest=NULL, verbose=FALSE) {
	!missing(zip.filename) || stop("zip.filename must be supplied")
	length(zip.filename) == 1 || stop("only support 1 zip.filename")
	
	if(is.null(dest)) {
		dest = getwd()
	}
	
	if( is.url(zip.filename) ) {
		destfile <- file.path(dest, basename(zip.filename))
		dlres <- download.file(zip.filename, destfile, quiet=!verbose)
		dlres == 0 || stop(sprintf("Could not download zip.filename: %s", zip.filename))
		zip.filename <- destfile
	}
	else if( !file.exists(zip.filename) ) {
		stop(gettextf("zipfile '%s' not found", zip.filename), domain = NA)
	}
	
	res <- NULL
	if( isWindows() ) {
		# I am sure that the utils::unzip should work fine on Windows, but this is here because
		# I haven't had a chance to test it yet... MJC, 2010-01-08
		# This code was called install.packages.R->zip.unpack
		if((unzip <- getOption("unzip")) != "internal") {
			res <- system(paste(unzip, "-oq", zip.filename, "-d", dest),
				   show.output.on.console = FALSE, invisible = TRUE)
		} else {
			res <- unzip(zip.filename, files=NULL, exdir=dest)
		}
	}
	else {
		res <- utils::unzip(zip.filename, exdir=dest)
		# unzip <- getOption("unzip")
		# system(paste(unzip, "-q", zip.filename, "-d", dest))
	}

	# if a set of local files are zipped using 'compress' from OSX finder, 
	# you can get special files included in the zip. these should be removed.
	mocosx.system.files <- grep("__MACOSX", res, value=TRUE)
	if( length(mocosx.system.files) > 1 ) {
		res <- setdiff(res, mocosx.system.files)
		
		macosx.dir <- unique(dirname(mocosx.system.files))
		unlink(macosx.dir, recursive=TRUE)
	}
	
	if( verbose )
		cat("Found these files in the zip archive:\n", paste(res, collapse="\n"), "\n", sep="")
	
	return( res )
}
# CHANGELOG
# 2010-01-08: v1
# 2011-10-19:
# - support added for URL's
