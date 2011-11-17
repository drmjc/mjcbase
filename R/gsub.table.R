## Function to quickly do a gsub on a table.
##
## CAUTION: i writes the file out to perl and then reads it back in. Thus column
## classes may change types from this method!
##
## Mark Cowley, 12 Nov 2004
##
gsub.table <- function(pat, repl, x) {
	if(missing(pat)) stop("missing pattern - see gsub")
	if(missing(repl)) stop("missing replacement - see gsub")
	if(missing(x)) stop("missing data - see gsub")

	rnames <- rownames(x)
	write.delim(x, "/tmp/ABCDEFGHIJK.txt")
	system(paste("perl -pi -e \'s/", pat, "/", repl, "/g' /tmp/ABCDEFGHIJK.txt", sep=""))
	x <- read.delim("/tmp/ABCDEFGHIJK.txt", as.is=T)
#	system("rm /tmp/ABCDEFGHIJK.txt")
	rownames(x) <- rnames

	x
}
