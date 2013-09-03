#' directory listing of an FTP directory.
#' 
#' Uses ncftpls, and via ftp.opts and ls.opts, you can specify a range of options. see examples,
#' and type ncftpls on the commandline to see the full range of options. ncftpls behaves
#' differently depending on whether there's a trailing slash. in this \R version, all URL's
#' will have a trailing slash.
#'
#' @section Warning:
#' on OSX and Ubuntu, the '-g' and '-gg' recursive modes don't work. untested on CentOS.
#' '-R' mode does work, but produces a long listing. note '-R -1' does not recurse.
#' 
#' @section TODO:
#' implement recursive, by in the back end, doing ls.opts="-l" & selecting directories by "^d"
#' 
#' @param url the full url to the directory. need trailing '/' to get just the filenames.
#' @param ftp.opts a non-NULL character vector of FTP options. see ncftpls help
#' @param ls.opts a non-NULL character vector of ls options. see ncftpls help
#' @param pattern an optional regular expression.  Only file names which match the 
#'  regular expression will be returned. The pattern is applied to the filename, not
#'  the whole URL.
#' @param full.names logical: if \code{TRUE}, the ftp url is prepended to the file
#'  names to give a fully specified ftp url. If \code{FALSE}, the file names, (rather
#'  than FTP URL's) are returned. Added for compatibility with \code{\link{dir}}.
#' 
#' @return a chracter vector of output. usually a file listing
#' 
#' @author Mark Cowley, 2013-05-21
#' @export
#' @examples
#' # connect to port 10021 of Garvan's FTP server & do a listing
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-1")
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-1", full.names=TRUE)
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip", ftp.opts="-P 10021", ls.opts="-1")
#' 
#' # recursive, long listing
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-R")
#' 
#' # recursive, short listing (doesn't work)
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-1 -g")
#' 
#' #  As above, but append a "/" character to directory pathnames. (doesn't work; doesn't add trailing slash...)
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/", ftp.opts="-P 10021", ls.opts="-1 -gg")
#' 
#' # test patterns
#' ftpdir("ftp://ftp.pwbc.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", pattern="rn4")
#' 
ftpdir <- function(url, ftp.opts="", ls.opts="", pattern=NULL, full.names=FALSE) {
	if( !grepl("/$", url) ) url <- paste0(url, "/")
	cmd <- sprintf("ncftpls %s %s %s", ls.opts, ftp.opts, url)
	files <- system(cmd, intern=TRUE)
	if( !is.null(pattern) ) {
		files <- grep(pattern, files, value=TRUE)
	}
	if( full.names ) {
		files <- file.path(sub("/$","",url), files)
	}
	files
}
# CHANGELOG
# - 2013-06-13: added full.names & trailing slash.
# 2013-06-19
# - add pattern argument, for consistency with dir()
# 