#' Get/set the column classes of matrix-like data
#' 
#' \code{colclasses} returns the classes of each column. It's quite
#' useful for checking whether \code{factor}s snuck into a \code{data.frame}.  All
#' columns in a \code{matrix} have the same type, but \code{data.frame}'s may
#' have mixed column types.
#' 
#' \code{colclasses<-} sets the column classes in \code{x}. Remember that \code{matrix}
#' columns are all of the same class, \code{data.frame} can have multiple classes.
#' 
#' @param x a \code{matrix}, or \code{data.frame}
#' 
#' @return \code{colclasses}: a \code{character vector} of column classes, with the same
#'   length as the number of columns. Typical classes include \code{numeric},
#'   \code{character}, \code{logical}, \code{factor}, \dots{} 
#' 
#' @author Mark Cowley, 18 Nov 2004
#' @export
#' @rdname colclasses
#' @aliases colclasses
#' @keywords attribute
#' 
#' @examples
#' # colnames getter example
#' x <- as.data.frame(cbind(1:5, LETTERS[1:5], rep(TRUE, 5)), stringsAsFactors=FALSE)
#' colclasses(x)                               
#' y <- as.data.frame(cbind(1:5, LETTERS[1:5], rep(TRUE, 5)), stringsAsFactors=TRUE)
#' colclasses(y)
colclasses <- function(x) {
  if( is.data.frame(x) | is.matrix(x) ) {
    res <- character(length=ncol(x))
    for(i in 1:ncol(x)) {
      res[i] <- class(x[,i])
    }
    return(res)
  }
  else {
    return(class(x))
  }
}


#' @param value a character vector of new column classes. if \code{length(value) < ncol(x)}, \code{value} is recycled
#' 
#' @return \code{colclasses<-}: return \code{x} with different column classes.
#' 
#' @export
#' @rdname colclasses
#' @aliases 'colclasses<-'
#' @usage colclasses(x) <- value
#' 
#' @examples
#' # colnames setter example
#' y <- as.data.frame(cbind(1:5, LETTERS[1:5], rep(TRUE, 5)), stringsAsFactors=TRUE)
#' colclasses(y)
#' colclasses(y) <- c("numeric", "character", "logical")
#' colclasses(y)
#'
'colclasses<-' <- function(x, value) {
    value <- recycle(value, ncol(x))
    oldclass <- class(x); olddn <- dimnames(x)
    x <- as.data.frame(x, stringsAsFactors=FALSE)
    for(i in 1:ncol(x)) {
        x[,i] <- as.character(x[,i])
        if(value[i] != "character")
            class(x[,i]) <- value[i]
    }
    if( all(colclasses(x) == colclasses(x)[1]) && oldclass == "matrix" )
        x <- as.matrix(x)
    dimnames(x) <- olddn

    x
}
# CHANGELOG
# 11 April 2006: v1
# update 2011-03-29
