#' merge a set of numbers into a range.
#' 
#' Useful for working out what is the minimal information to describe a series
#' of numbers
#' 
#' @note This is almost certainly replaceble using code from IRanges package,
#' and Rle obects.
#' 
#' @param x a numeric \code{vector}
#' @param gaplength allow a gap within a run. default=0
#' 
#' @author Mark Cowley
#' @export
#' @examples
#' x <- c(1, 2, 3, 4, 5, 9)
#' range_merge(x)
#' #   from to
#' # 1    1  5
#' # 2    9  9
#' range_merge(x, gaplength=3)
#' #   from to
#' # 1    1  9
#' range_merge(x, gaplength=2)
#' #   from to
#' # 1    1  5
#' # 2    9  9
range_merge <- function(x, gaplength=0) {
	x <- unique( sort(x, na.last = NA) )

	if( length(x) == 0 )
		return( NULL )

	res <- as.data.frame(matrix(0, nrow=length(x), ncol=2))
	idx <- 1 # index to remember which row of res we are up to.

	#
	# find the maximal contiguous runs of indices
	#
	runstart <- x[1]
	runend <- x[1]

	if( length(x) == 1 ) {
		res[idx,] <- c(runstart, runend)
		idx <- idx + 1 # warning, idx is now pointing to a non-existent row
	}
	else {
		for(i in 2:length(x)) {
			if( (runend + 1 + gaplength) >= x[i] ) ## i.e. runend = 3, x[i] = 4
				runend <- x[i]
			else {
				res[idx,] <- c(runstart, runend)
				idx <- idx + 1
				runstart <- x[i]
				runend <- x[i]
			}

			if( i == length(x) ) {
				res[idx,] <- c(runstart, runend)
				idx <- idx + 1
			}
		}

		if( idx <= nrow(res) ) {
			res <- res[c(1:(idx-1)), ]
		}
	}


	colnames(res) <- c("from", "to")

	return( res )
}
