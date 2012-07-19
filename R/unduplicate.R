#' unduplicate elements in a vector
#' 
#' Similar to \code{\link{make.unique}}, ensure that all elements in \code{x}
#' are unique. Note that duplicate can mean any number of repetitions, not just 2.
#' This differs from \code{\link{make.unique}} in that \emph{all} instances of a duplicated
#' element are tagged with a numeric suffix, as opposed to the 2:n elements that get
#' tagged by \code{\link{make.unique}}. see examples 
#' 
#' @param x a character vector
#' @param sep the seperator between \code{<original><sep><integer>}
#' 
#' @return a character vector, \code{length(x)}, where all elements are unique
#' 
#' @author Mark Cowley, 2012-05-02
#' @seealso \code{\link{make.unique}}
#' @export
#' 
#' @examples
#' x <- c("a","a","a","b","b")
#' unduplicate(x)
#' # [1] "a.1" "a.2" "a.3" "b.1" "b.2"
#' # compared to make.unique where the first 'a' and 'b' are unchanged.
#' make.unique(x)
#' # [1] "a"   "a.1" "a.2" "b"   "b.1"
#' unduplicate(letters[1:6])
#' # [1] "a" "b" "c" "d" "e" "f"
#' unduplicate(c(x,NA, NA, NA))
#' # [1] "a.1" "a.2" "a.3" "b.1" "b.2" NA    NA    NA   
unduplicate <- function(x, sep=".") {
	res <- x

	if( length(anyDuplicated(x)) > 0 ) {
		dups <- unique(x[duplicated(x)])
		for(dup in dups) {
			idx <- which(x == dup)
			res[idx] <- sprintf("%s%s%d", dup, sep, 1:length(idx))
		}
	}
	
	res
}
