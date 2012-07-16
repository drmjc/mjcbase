#' is alphanumeric?
#'
#' @param x a \code{character} \code{vector}, or an object which can be coerced
#'	to a \code{character} \code{vector}.
#' 
#' @return logical, length 1
#' @author Mark Cowley, 2012-07-16
#' @export
is.alnum <- function(x) {
	return( length(grep("[^a-zA-Z0-9_]", as.character(x))) == 0 )
}

#' is alphabetical?
#' 
#' @inheritParams is.alnum
#' @return logical, length 1
#' @author Mark Cowley, 2012-07-16
#' @export
is.alpha <- function(x) {
	return( length( grep("[^A-Za-z]", as.character(x) ) ) == 0 )
}

#' convert to alphanumeric
#' 
#' @inheritParams is.alnum
#' @return undocumented
#' @author Mark Cowley, 2012-07-16
#' @export
to.alnum <- function(x) {
	for( j in 1:(length(x)) ) {
		if(!is.alnum(x[j])) {
			v <- to.char.array(x[j])
			for(i in 1:length(v)) {
				if(!is.alnum(v[i]))
					x[j] <- paste(substr(x[j],1,i-1),"_", substring(x[j], i+1), sep="")
			}
		}
	}
	x
}
