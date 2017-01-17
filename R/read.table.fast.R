#' read.delim fast, on a subset of columns
#' 
#' If you have a huge file & are only interested in some of the columns,
#' \code{read.delim.fast} can be much much faster. It depends on how many
#' columns there are. If you have a VERY fast file, then \code{\link{read.table.fast}}
#' and subsetting columns after may still be faster.
#' 
#' Some testing on a 1134514 x 32 column file (an Illumina Omni1M TXT file),
#' extracting 5 columns of interest saw a 4.8x speedup (8.5 vs 40.5 sec).
#' 
#' @section timing:
#' a <- read.delim.fast(file, skip=11, nrows=10, columns=1:5)                   \cr
#' system.time(a <- read.delim.fast(file, skip=11, nrows=1134514, columns=1:5)) \cr
#' #  user  system elapsed                                                   \cr
#' # 7.550   2.240   8.457                                                   \cr
#' system.time(a <- read.delim(file, skip=11, nrows=1134514)[,1:5])             \cr
#' #   user  system elapsed                                                  \cr
#' # 39.070   1.190  40.451                                                  \cr
#'
#' @note WARNING
#' In general this code works really well when there are the same number of column names
#' as there are columns. It works great on tsv and csv files not produced by \R, since
#' they almost always have the same number of colnames as columns.\cr
#' \R likes to do some strange things with \code{\link{write.table}} and \code{\link{write.csv}},
#' where if row.names=TRUE, then there will be 1 fewer colnames as there are columns of
#' data. When using \code{\link{read.table}} and \code{\link{read.csv}}, this signals that 
#' the 1st column should be the \code{row.names}, then the subsequent columns are cols \code{1-N}.
#' Since this code uses GNU \code{cut}, it's not aware of this & will likely shift the colnames and columns. 
#' 
#' @inheritParams utils::read.delim
#' @param columns a character vector of column names to import; or a numeric vector 
#'  of column indices to import; or a logical vector same length as ncol(file)
#' @return a \code{data.frame} containing just the columns of interest, just like
#' you'd get from \code{\link{read.table}}, only much faster.
#' 
#' @author Mark Cowley, 2012-06-22
#' @rdname read.table.fast
#' @aliases read.table.fast
#' @export
#' @importFrom utils read.table
#' @docType methods
#'
#' @examples
#' ##
#' ## read.table.fast examples\cr
#' ##
#' test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
#' tf <- tempfile()
#' 
#' write.table(test1, tf)
#' # [1] "\"a\" \"b\" \"c\" \"d\""               
#' # [2] "\"1\" \"a\" \"A\" 1 0.379498970282498" 
#' # note the missing leading blank -- cut is NOT happy with this
#' readLines(tf)
#' read.table(tf)[,1:4]
#' # read.table.fast(tf, columns=1:4)
#' 
#' unlink(tf)
#' 
read.table.fast <- function(file, skip=0, nrows=-1, header=TRUE, row.names, sep="", ..., columns=NULL) {
	if( sep == "" ) stop("This function won't work for sep=\"\", since there is 1 less column in header wrt the table & this is too hard to code at this time of night.")
	if( missing(row.names) ) row.names <- NULL
	if( nrows == 0 && !header ) return(data.frame())
	
	top <- read.table(file, skip=skip, nrows=5, header=header, row.names=row.names, sep=sep, ...)
	all.columns <- colnames(top)

	if( is.null(columns) ) {
		return( read.table(file, skip=skip, nrows=nrows, header=header, row.names=row.names, sep=sep, ...))
	}
	else if( is.character(columns) ) {
		header <- TRUE
		if( !all(columns %in% all.columns) ){
			msg <- sprintf("some columns not found: %s\nValid column names are:\n%s", paste(setdiff(columns, all.columns), collapse=", "), paste(all.columns	, collapse="\n"))
			stop(msg)
		}
		columns <- match(columns, all.columns)
	}
	else if( is.numeric(columns) ) {
		min(columns) >= 1 || stop("columns must be >= 1")
		max(columns) <= length(all.columns) || stop( paste("you've asked for too many columns. max is", length(all.columns)) )
	}
	else if( is.logical(columns) ) {
		length(columns) == length(all.columns) || stop( paste("if columns is logical, it must have length", length(all.columns)) )
		columns <- which(columns)
	}
	else {
		stop("unsupported values for 'columns'")
	}
	if( !is.null(row.names) ) {
		if( row.names == 1) columns <- c(1, columns+1)
		else if ( is.character(row.names) && row.names == all.columns[1] && ! row.names %in% columns)  columns <- c(1, columns+1)
		else stop("unsupported row.names setting. try leaving it as missing or NULL, and fixing the row.names after import.")
	}
	
	#
	# work out the delimeter to use for cut.
	# 
	delim <- ""
	if( sep == "\t" ) {
		delim <- ""  # ie use cut's default
	}
	else if( sep == "" ) { # need to be careful with having 1 fewer colnames than columns
		delim <- '-d " "'
	}
	else if( sep == "," ) {
		delim <- '-d ","'
	}
	else {
		delim <- sprintf("-d %s", sep)
	}
	
	#
	# how many rows to read in? if all, then just use echo
	#
	head.cmd <- 
		if( is.na(nrows) || is.null(nrows) || !is.numeric(nrows) || nrows < 0 ) "cat"
		else if( nrows == 0 && header ) "head -n 1"
		else if( nrows == 0 && !header ) stop("Should never see this.")
		else paste("head -n", as.character(nrows + header))
	
	tmp <- tempfile()
	
	#
	# if there's 1 fewer column names than columns, then row.names is the 1st column, and
	# the indices specified in the 'columns' vector should actually by +1 as far as cut is concerned.
	# get the column names
	# 
	cmd <- sprintf(
		"tail -n +%d %s | %s | cut %s -f %s > %s", 
		skip+1, # the tail -n +%d syntax means start at line %d...
		shQuote(Sys.glob(file)), 
		head.cmd, 
		delim,
		paste(columns, collapse=","), 
		shQuote(tmp)
	)
	# print(cmd)
	system(cmd)
	
	res <- read.table(tmp, header=header, sep=sep, row.names=row.names, ...)
	res
}
# CHANGELOG
# 2012-07-06: GNU head accepts head -n -0 (meaning head all, but the last 0 lines), but
# BSD head doesn't accept head -n X, where X <= 0.

#' @rdname read.table.fast
#' @aliases read.delim.fast
#' @export
#' 
#' @examples
#' ##
#' ## read.delim.fast examples\cr
#' ##
#' \dontrun{
#' file <- "~/tmp/ASCAT/TXT/ICGC_ABMP_20100506_11_ND_5486142165_R03C01.txt"
#' system.time(
#'   a <- read.delim.fast(file, skip=11, nrows=1134514, columns=1:5)
#' )
#' 
#' system.time(
#'     a <- read.delim.fast(file, skip=10, nrows=1134514, check.names=FALSE, columns=c("SNP Name", "Sample ID", "Chr", "Position", "B Allele Freq", "Log R Ratio"))
#' )
#' }
#' 
#' test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
#' tf <- tempfile()
#' # pwbc::write.delim default keeps ncol or data and colnames in sync
#' write.delim(test1, tf)
#' readLines(tf)
#' # [1] "a\tb\tc\td"
#' # [2] "a\tA\t1\t0.379498970282498" 
#' # ...
#' # note no rownames or leading blank in column. cut is happy
#' read.delim(tf)[,1:3]
#' read.delim.fast(tf, columns=1:3)
#' 
#' unlink(tf)
read.delim.fast <- function(file, header = TRUE, sep = "\t", quote='"', dec=".", fill = TRUE, comment.char="", ..., columns=NULL) {
	read.table.fast(file = file, header=header, 
		sep=sep, quote=quote, dec=dec, fill=fill, comment.char=comment.char, ..., columns=columns
	)
}


#' @rdname read.table.fast
#' @aliases read.csv.fast
#' @export
#' 
#' @examples
#' ##
#' ## read.csv examples
#' ##
#' test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
#' tf <- tempfile()
#' 
#' write.csv(test1, tf)
#' readLines(tf)
#' # [1] "\"\",\"a\",\"b\",\"c\",\"d\""          
#' # [2] "\"1\",\"a\",\"A\",1,0.379498970282498" 
#' # ...
#' # ^^ note the leading blank cell. 'cut' is happy
#' read.csv(tf)[,1:4]
#' read.csv.fast(tf, columns=1:4)
#' # both get this equally wrong by putting the old rownames into the new column1
#' read.csv(tf)[,1:5]
#' read.csv.fast(tf, columns=1:5)
#' # see? there should only be 4 columns
#' read.csv(tf, row.names=1)[,1:4]
#' read.csv.fast(tf, columns=1:4, row.names=1)
#' read.csv.fast(tf, columns=1:4, header=FALSE, nrows=2)
#' #   V1 V2 V3 V4
#' # 1 NA  a  b  c
#' # 2  1  a  A  1
#' read.csv.fast(tf, columns=1:4, header=TRUE, nrows=2)
#' #   X a b c
#' # 1 1 a A 1
#' # 2 2 b B 2
#' read.csv.fast(tf, columns=1:4, header=TRUE, nrows=2, row.names=1)
#' #   a b c      d
#' # 1 a A 1 -1.569
#' # 2 b B 2  0.976
#' read.csv.fast(tf, columns=c(1,2,4), header=TRUE, nrows=2, row.names=1)
#' #   a b      d
#' # 1 a A -1.569
#' # 2 b B  0.976
#' 
#' unlink(tf)
read.csv.fast <- function(file, header=TRUE, sep = ",", quote='"', dec=".", comment.char="", ..., columns=NULL) {
	read.table.fast(file = file, header=header, 
		sep=sep, quote=quote, dec=dec, comment.char=comment.char, ..., columns=columns
	)
}
