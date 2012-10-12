#' zip the contents of a directory.
#' 
#' Note that the directory name itself will not be included in the directory.
#' The path to all files will be relative to the specified directory.
#' 
#' @param dir the parent directory, whose files are to be zipped.
#' @param zip.file the zip file to be created
#' @param exclude.patterns an optional vector of patterns to be excluded. eg
#'   "\*.zip" NB: use \* to allow zip to do the pattern exclusion rather than
#'   shell
#' @param verbose logical: print the zip command. Default=FALSE
#' 
#' @return a zip file is created, containing all files within \code{dir}
#' 
#' @note This uses system commands, and needs to be able to find the gnu zip
#'   program. You can specify the zip program using \code{options("zip") <-
#'   "/path/to/zip"}. If this is not specified, then we assume that the shell
#'   can find a program called \code{zip} on the path. On ubuntu and OSX this
#'   is \sQuote{/usr/bin/zip}.
#' @author Mark Cowley, 2010-04-14
#' @seealso \code{\link[utils]{unzip}}
#' @keywords utils
#' @examples
#' 
#' dir <- tempdir()
#' writeLines(letters, file.path(dir, "a.txt"))
#' writeLines(LETTERS, file.path(dir, "b.txt"))
#' zipdir(dir, file.path(dir, "tmp.zip"), verbose=TRUE)
#' cmd <- paste("unzip -l", file.path(dir, "tmp.zip"), collapse=" ")
#' system(cmd)
#' f <- c(file.path(dir, "a.txt"), file.path(dir, "b.txt"), file.path(dir, "tmp.zip"))
#' unlink(f)
#'
#' @export
zipdir <- function(dir, zip.file, exclude.patterns=NULL, verbose=FALSE) {
	!missing(dir) && !is.null(dir) && (length(dir)==1) && file.exists(dir) && is.dir(dir) || stop("dir must be a single directory")
	!missing(zip.file) && !is.null(zip.file) && (length(zip.file)==1) && file.exists(dirname(zip.file)) || stop("zip.file must be a valid path to a zip file")
	zip.file <- get.full.path(zip.file)
	
	path.to.zip <- getOption("zip")
	if(is.null(path.to.zip)) {
		path.to.zip <- system("which zip", intern=TRUE) # Sys.which doesn't exist in R 2.5, otherwise i'd use that.
		nchar(path.to.zip) != 0 || stop("Can't find a suitable zip program on your path. See ?zipdir for more details.")
	}

	if( !is.null(exclude.patterns) ) {
		# todo: ensure the exclude.patterns are properly escaped for the shell.
		exclude.patterns <- paste("-x", exclude.patterns)
		exclude.patterns <- paste(exclude.patterns, collapse=" ")
	} else {
		exclude.patterns <- ""
	}
	# -q = quiet; -X = --no-extra; -x = exclude various files; -r = recurse into subdirectories (& should be the last arg).
	cmd <- sprintf("cd %s && %s -q -X %s -r %s *", shQuote(dir), path.to.zip, exclude.patterns, shQuote(zip.file))
	
	if( verbose ) cat(cmd, "\n")
	success <- system(cmd, intern=FALSE, wait=TRUE) == 0
	if( success && verbose ) cat(shQuote(zip.file), "successfully created.\n")
	
	invisible( success )
}
# CHANGELOG
# 2010-04-14: v1
# 2011-04-28: robustified the code. check if zip can be found. verbose output. Rd made.
# 2012-10-12: bug fix; force path expansion of the zip.file, rather than normalizePath which does nothing for an argument named 'simple.zip' where the ./ is implicit.
