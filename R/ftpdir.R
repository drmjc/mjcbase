#' directory listing of an FTP directory.
#' 
#' Uses ncftpls, and via ftp.opts and ls.opts, you can specify a range of options. see examples,
#' and type ncftpls on the commandline to see the full range of options.
#'
#' @section Warning:
#' on OSX and Ubuntu, the '-g' and '-gg' recursive modes don't work. untested on CentOS.
#' '-R' mode does work, but produces a long listing. note '-R -1' does not recurse.
#' 
#' @param url the full url to the directory. need trailing '/' to get just the filenames.
#' @param ftp.opts a non-NULL character vector of FTP options. see ncftpls help
#' @param ls.opts a non-NULL character vector of ls options. see ncftpls help
#' 
#' @return a chracter vector of output. usually a file listing
#' 
#' @author Mark Cowley, 2013-05-21
#' @export
#' 
#' @examples
#' # connect to port 10021 of Garvan's FTP server & do a listing
#' ftpdir("ftp://pwbcftp01.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-1")
#' 
#' # recursive, long listing
#' ftpdir("ftp://pwbcftp01.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-R")
#' 
#' # recursive, short listing (doesn't work)
#' ftpdir("ftp://pwbcftp01.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/chip/", ftp.opts="-P 10021", ls.opts="-1 -g")
#' 
#' #  As above, but append a "/" character to directory pathnames. (doesn't work; doesn't add trailing slash...)
#' ftpdir("ftp://pwbcftp01.garvan.unsw.edu.au/pub/genepattern/modules/NormalizeAffymetrixST/", ftp.opts="-P 10021", ls.opts="-1 -gg")
#' 
ftpdir <- function(url, ftp.opts="", ls.opts="") {
	cmd <- sprintf("ncftpls %s %s %s", ls.opts, ftp.opts, url)
	system(cmd, intern=TRUE)
}
