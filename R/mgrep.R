#' @title multi-grep methods
#' 
#' @description a collection of multi-grep methods which make use of \emph{multiple} patterns,
#' as opposed to \code{\link[base]{grep}}'s \emph{single} pattern.
#' 
#' @details \code{mgrep}: \code{mgrep} takes a vector of \code{patterns}, and searches for each within
#' \code{x}, returning either the matching indices into \code{x}, or values (if \code{value=TRUE}),
#' for each pattern in a \code{list} of \code{length(patterns)}.
#' Just like \code{\link[base]{mget}} as the multi-version of \code{\link[base]{get}},
#' there is a \code{nomatch} parameter controlling what to do if no match for a particular 
#' pattern is found.
#' 
#' @param patterns a character vector of at least one pattern.
#' @param x	a character vector where matches are sought, or an object
#'   which can be coerced by \code{as.character} to a character vector.
#' @inheritParams base::grep
#' @param nomatch if a pattern isn't found, what should be returned for that element in the
#'  result \code{list}? default=\code{NA} which just reports an \code{NA}. \code{NULL} is
#' allowed, in which case a \code{numeric(0)} or \code{character(0)} (if \code{value=TRUE})
#' is put in the corresponding element of the list.
#' 
#' @return \code{mgrep}: a \code{list} with one element per pattern in the \code{patterns} vector.
#'   Each element of this \code{list} is the value returned by \code{\link{grep}} for each pattern,
#'   OR \code{nomatch} if nothing is found. NB this is different from the default
#'   behaviour of \code{\link{grep}}, which returns \code{numeric(0)} when no match is found.
#' 
#' @author Mark Cowley, 23 Sept 2005
#' @rdname mgrep
#' @export
#' @examples
#' # use the state.name example data
#' mgrep(c("New","^V", "Iran"), state.name)
#' mgrep(c("New","^V", "Iran"), state.name, value=TRUE)
#' 
#' # nomatch
#' mgrep(c("New","^V", "Iran"), state.name, nomatch=NULL)
#' mgrep(c("New","^V", "Iran"), state.name, value=TRUE, nomatch=NULL)
mgrep <- function( patterns, x, ignore.case = FALSE, perl = FALSE,
                   value = FALSE, fixed = FALSE, useBytes = FALSE, invert=FALSE,
                   nomatch = NA ) {

    res <- lapply(patterns, function(pattern) {
        grep(pattern, x, ignore.case = ignore.case, perl = perl,
             value = value, fixed = fixed, useBytes = useBytes, invert = invert)
    })
    names(res) <- patterns

    resl <- sapply(res, length)
    if( any(resl == 0) && !is.null(nomatch) ) {
		res[resl == 0] <- nomatch
	}
	# else if( any(resl == 0) && is.null(nomatch) ) {
	# ^^ not needed as data is already numeric(0), or character(0) depending
	# on the value of \code{value}

    return(res)

}
# CHANGELOG
# 2012-05-02: updated doc & made code more robust wrt nomatch


#' @details \code{mgrepl}: grep for multiple \code{patterns}, and return logical if each pattern is present
#' in any value of \code{x}
#' 
#' @inheritParams grepl
#' @author Mark Cowley, 2011-02-18
#' @export
#' @rdname mgrep
#' @return \code{mgrepl}: a logical vector, \code{length = length(patterns)}, indicating whether each
#'  pattern was found in \code{x}
#' @examples
#' # use the state.name example data
#' mgrepl(c("New","^V", "Iran"), state.name)
#' #  New    ^V  Iran 
#' # TRUE  TRUE FALSE 
#' 
mgrepl <- function( patterns, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE) {
	res <- mgrep(patterns, x, ignore.case=ignore.case, perl=perl, fixed=fixed, nomatch=NULL)
	sapply(res, length) > 0
}
# CHANGELOG
# 2012-05-02 lapply -> sapply; changed doc to use @inheritParams

#' @details \code{grepf}: grep -f for R. ie return the elements of \code{x} 
#' which match \emph{any} of the \code{patterns}.
#' 
#' @inheritParams grepl
#' 
#' @return \code{grepf}: if \code{value=FALSE}, a \code{numeric} vector of 
#' indices in \code{1:length(x)} which match \emph{any} of the \code{patterns},
#' or, if \code{value=TRUE}, return a \code{character} vector of the elements
#'  of \code{x} which match \emph{any}
#' of the patterns.
#' 
#' @author Mark Cowley, 2011-02-18
#' @rdname mgrep
#' @export
#' @examples
#' # use the state.name example data
#' grepf(c("New","^V", "Iran"), state.name)
#' # [1] 29 30 31 32 45 46
#' grepf(c("New","^V", "Iran"), state.name, value=TRUE)
#' grepf(c("New","^V", "Iran"), state.name, invert=TRUE)
grepf <- function( patterns, x, ignore.case = FALSE, perl = FALSE,
                   value = FALSE, fixed = FALSE, useBytes = FALSE, invert=FALSE ) {
	res <- rep(FALSE, length(x))
	for(pattern in patterns) {
		tmp <- which(!res) # reduce needless calculations
		tmp2 <- grepl(pattern, x[tmp], perl=perl, fixed=fixed, useBytes=useBytes)
		if( any(tmp2) ) {
			res[ tmp[tmp2] ] <- TRUE
		}
		if( all(res) ) break
	}
	# res is TRUE/FALSE for each element of x
	if( invert ) res <- !res
	
	if( value ) res <- x[res]
	else res <- which(res)

    return(res)
}

#' @details \code{grepfl}: combo of \code{grepf} + \code{grepl} ie which 
#' elements of \code{x} match \code{>=1} of the \code{patterns}, as
#' a logical vector
#' 
#' @inheritParams grepl
#' 
#' @return \code{grepfl}: a \code{logical} vector, of length\code{=length(x)}, 
#' indicating whether each of the elements in \code{x} match \emph{any} of the 
#' \code{patterns}. if \code{invert=TRUE}, then the logical vector indicates 
#' which elements did not match \emph{any} of the \code{patterns}.
#' 
#' @author Mark Cowley, 2011-02-18
#' @rdname mgrep
#' @export
#' @examples
#' # use the state.name example data
#' grepfl(c("New","^V", "Iran"), state.name)
#' grepfl(c("New","^V", "Iran"), state.name, invert=TRUE)
grepfl <- function( patterns, x, ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE, invert=FALSE ) {
	tmp <- grepf(patterns=patterns, x=x, ignore.case=ignore.case, perl=perl, value=FALSE, fixed=fixed, useBytes=useBytes, invert=invert)
	res <- rep(FALSE, length(x))
	res[tmp] <- TRUE
	res
}
# CHANGELOG:
# 2012-05-02 dropped the unused value parameter
