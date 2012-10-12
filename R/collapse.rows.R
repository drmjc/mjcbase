#' collapse rows
#' 
#' collapse/uncollapse the rows in a \code{data.frame}
#' 
#' @section Collapsed data:
#' Collapsed data means a \code{data.frame} with at least 1 column whose values
#' are \code{sep} delimited. example:\cr
#' * a cell of data with multiple gene symbols "Ankrd11|Galnt2"\cr
#' * a cell of data with multiple GO terms, eg "GO:00001 /// GO:00002 /// GO:00003"\cr
#' * a cell of data with multiple attributes, eg "TD, ND, CD"
#' These type of data are very common when there are multiple values per \code{key}.
#' 
#' @section uncollapse.rows:
#' uncollapse.rows takes collapsed data, and increases the number of rows, such that these
#' data have 1 element per row. so 1 row with "Ankrd11|Galnt2" becomes 2 rows with
#' "Ankrd11" and "Galnt2" for example. Thus changing it from n:1 to 1:1. 
#' 
#' All columns that are not specified in \code{cols2uncollapse} will be repeated.
#' If you have just 1 column to uncollapse, then only that column will be changed.
#' If you have more than 1 column to expand, then within those rows that need uncollapsing,
#' all specified columns MUST have the same number of elements.
#' Eg consider a \code{data.frame} with 1 row per gene with 3 GO-term columns: 
#' GO.ID, GO.Name, GO.Evidence. For any given gene with 3 GO terms, there should
#' also be exactly 3 GO ID's, 3 GO Names and 3 GO term evidence codes. If there are different
#' numbers of elements found this will thow an error. @@TODO: allow a different number
#' of values per collapsed row.
#' 
#' Strongly suggest using this function to reverse the effects of
#' \code{\link{collapse.rows}}, using
#' the same arguments that were supplied to \code{\link{collapse.rows}} itself.
#' 
#' @param x a \code{data.frame}
#' @param key.column the column that must end up having one row per key.
#' numeric or character allowed.
#' @param cols2collapse which are the columns that you want to collapse. Often
#'   there are columns which will contain the same info repeated over and over,
#'   and you don't want these things to have the same word repeated N times.
#'   Just ignore these columns then, and only supply the column names of those
#'   columns that you want to be joined.
#' @param sep the seperator, such as \dQuote{, } or \dQuote{ // }
#' @param cols2uncollapse Which columns need uncollapsing? Must specify at least
#'  1 column (hint: this is the column that contains \code{sep} that you're trying to
#'  get rid of). If you specify >1 columns, then each cell in that row must have the
#' same number of code elements to be split.
#' @param max.nchar the maximum number of characters in each collapsed cell. if \code{NULL}
#'  then no filtering is performed, otherwise long strings will be truncated at \code{max.nchar-3}
#'  with \dots appended. if you intend to use \code{\link{uncollapse.rows}} later, and
#'  depend on there being the correct number of values found, then leave \code{max.nchar=NULL}
#' 
#' @section collapse.rows:
#' Take a \code{data.frame} which has rows that contain mostly the same info, but some
#' columns change. You want one row per unique value of \code{key} from \code{x[,key.column]},
#' and in the columns that contain non-equal data, collapse these values into a
#' single value, separated by \dQuote{, } or \dQuote{ // } for example.
#' 
#' @return \code{collapse.rows}: a \code{data.frame} with same num columns, but 
#'  only N rows corresponding to
#'  the N different values in the key column. alphanumerically sorted by key
#'  column.\cr
#'  \code{uncollapse.rows}:  a \code{data.frame} with same num columns, but with no 
#'  rows that have duplicate values in the \code{cols2uncollapse}.
#' 
#' @author Mark Cowley, 2009-01-07
#' @export
#' @rdname collapse.rows
#' 
#' @seealso \code{\link{uncollapse.rows}}
#' @examples
#' df <- data.frame(
#'    Name=rep(LETTERS[1:3], each=3), 
#'    Description=rep(letters[1:3], each=3),
#'    Value=LETTERS[11:19],
#'    stringsAsFactors=FALSE
#' )
#' a <- collapse.rows(df, 1, 3)
#' a
#' uncollapse.rows(a, 1, 3)
#'
collapse.rows <- function(x, key.column=1, cols2collapse=NULL, sep=" // ", max.nchar=NULL) {

	if( is.character(key.column) )
		key.column <- match(key.column, colnames(x))
		
	if( is.character(cols2collapse) )
		cols2collapse <- match(cols2collapse, colnames(x))
	
	# other.columns <- setdiff(1:ncol(x), c(key.column, cols2collapse))

	if( any(is.na(x[,key.column])) ) {
		idx <- !is.na(x[,key.column])
		x <- x[idx, ]
	}

	unique.keys <- sunique(x[,key.column])
	
	idx <- match(unique.keys, x[,key.column])
	res <- x[idx, ]
	# res now has the correct number of rows, 
	
	if( !is.null(cols2collapse) ) {
		# kill the data in columns that needs to be combined together.
		res[, cols2collapse] <- NA
		hash <- vector2hashtable(x[,key.column])

		for(i in 1:length(unique.keys)) {
			key <- unique.keys[i]
			rows <- get(key, envir=hash)
			if( length(rows) == 1 )
				res[i, cols2collapse] <- x[rows, cols2collapse]
			else {
				for(append.column in cols2collapse) {
					tmp <- x[rows, append.column]
					if( alleq(tmp, na.rm=TRUE) )
						tmp <- tmp[!is.na(tmp)][1]
					else if (all(is.na(tmp)))
						tmp <- NA
					else if (any(is.na(tmp))) {
						tmp[is.na(tmp)] <- ""
						tmp <- paste(tmp, collapse=sep)
					}
					else
						tmp <- paste(tmp, collapse=sep)
					if( !is.null(max.nchar) && nchar(tmp) > max.nchar )
						tmp <- paste(str_left(tmp, max.nchar-3), "...", sep="")
					res[i, append.column] <- tmp
				}
			}
		}
	}
	
	rownames(res) <- 1:nrow(res)
	
	res
}
# CHANGELOG:
# 2012-07-24
# - bug fix: redundant values to collapse was not being checked properly
# - added max.nchar option to truncate long lines.
# 2012-08-15: bug fix in strleft -> str_left
# 2012-08-28: alleq(..., na.rm=TRUE)

#' @export
#' @rdname collapse.rows
uncollapse.rows <- function(x, cols2uncollapse=NULL, sep=" // ") {
	!is.null(cols2uncollapse) || stop("cols2uncollapse must be specified")
	# how many times should each row get repeated?
	a <- strsplit(x[, cols2uncollapse[1]], sep)
	Nreps <- sapply(a, length)
	res <- x[rep(1:nrow(x), Nreps), ]

	for( col in cols2uncollapse ) {
		a <- strsplit(x[,col], sep)
		tmp.Nreps <- sapply(a, length)
		all(tmp.Nreps == Nreps) || stop("For each row, he number of elements to be uncollapsed must be equal, for all cells specified within x[,cols2uncollapse]")
		res[,col] <- unlist(a)
	}

	res
}
# CHANGELOG
# 2011-12-19:
# - improved doc
# - dropped code that looked at key.column
# - improved robustness when > 1 columns to be uncollapsed.
# 