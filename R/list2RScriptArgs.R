#' convert a named list of name=value pairs into properly quoted Rscript parameters
#' 
#' Currently we support \dQuote{character}, \dQuote{numeric}, \dQuote{intetger} and \dQuote{logical}
#' data types. \code{NA} are OK, but not \code{NULL}.
#' 
#' @param x a named list, where each element has just 1 value
#' @return a chracter(1) which can be tacked onto the end of an Rscript commandline
#'   if any values in x are characters, they will be double quoted. each individual
#'   parameter name=value pair will be single quoted
#' @author Mark Cowley, 2011-08-18
#' @export
#' @examples
#' x <- list(a="C2CGP", b=1, c=TRUE, out="/mnt/ICGCPancreas")
#' list2RscriptArgs(x)
list2RscriptArgs <- function(x) {
    if( length(x) == 0 ) {
        ""
    }
    else {
        is.list(x) || stop("x must be a list")
		all(!sapply(x, is.na)) || stop("NA not supported")
        cl <- sapply(x, class)
        all(cl %in% c("numeric", "integer", "character", "logical", "NULL")) || stop("Unsupported argument type:", paste(cl, collapse=", "))
        # all((tmp<-sapply(subset(x,!is.na(x)), length))<=1)        || stop("all entries in x must be of length 1: ", paste(tmp, collapse=", "))
        all((tmp <- sapply(x, length)) <= 1)        || stop("all entries in x must be of length 1: ", paste(tmp, collapse=", "))
        
        .trim <- function(x) { 
            sub("^[ \t]+", "", sub("[ \t]+$", "", x))
        }
        .dquote <- function(x, trim=FALSE) {
            if(trim) x <- .trim(x)
            paste('"', x, '"', sep="")
        }
        .squote <- function(x, trim=FALSE) {
            if(trim) x <- .trim(x)
            paste("'", x, "'", sep="")
        }
	
		if( any(cl == "character") ) {
	        for(i in which(sapply(x, is.character))) {
	            x[[i]] <- .dquote(x[[i]])
	        }
		}

        # for(i in seq(along=which(sapply(x, is.null)))) {
        #     x[[i]] <- "NULL"
        # }
        # suppressWarnings(
        #             for(i in seq(along=which(sapply(x, is.na)))) {
        #                 x[[i]] <- "NA"
        #             }
        #         )
        # for(i in seq(along=which(sapply(x, is.logical)))) {
        #     x[[i]] <- if(x[[i]] == TRUE) "TRUE" else "FALSE"
        # }
        for(i in seq(along=x)) {
            x[[i]] <- paste(names(x)[i], "=",x[[i]], sep="")
            x[[i]] <- .squote(x[[i]])
        }
        res <- unlist(x)
        res <- paste(res, collapse=" ")
        res
    }
}
