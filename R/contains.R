## more useful version of grep that returns True or False, depending on whether the pattern
## exists in 'x'.
##
## Mark Cowley, 3 June 2005
##
contains <- function(pattern, x, ignore.case=FALSE, extended=TRUE, perl=FALSE, value=FALSE, fixed=FALSE) {
	stop("Deprecated in favour of grepl as of R 2.11 & 2010-06-09.\n")
	# return(length(grep(as.character(pattern), as.character(x), ignore.case, extended, perl, value, fixed)) > 0)
}
