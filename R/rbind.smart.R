#' A smarter rbind
#' \code{rbind} 2 matrix-like-objects even if they have different numbers of
#' columns.  It's a bit like \code{merge()} but via rowbindings, not
#' colbindings. It produces a result which as \code{union(colnames(a),
#' colnames(b))}, and fills in missing data with \code{NA}. See details.
#' 
#' The resulting data.frame will have \code{nrow(x)} + \code{nrow(y)} rows, and
#' \code{length(union(colnames(x), colnames(y)))} columns.
#' 
#' If x and y contain the same colnames, then \code{rbind.smart} ==
#' \code{rbind}.
#' 
#' If x and y contain partially overlapping colnames, then the result will be
#' the union of all colnames, with NA's filled in where appropriate.
#' 
#' If x and y contain no overlapping colnames, then the result will have x in
#' top left and y in bottom right, filled in with NA's. as in: \preformatted{ x
#' : X; y: Y rbind.smart(x, y) -> X NA NA Y } Naming rules: column classes from
#' \code{x} take precedence over those from \code{y}, and the colnames of
#' result will be all of the colnames from x, then the colnames from y that
#' were not also in x at the end.
#' 
#' @param x,y matrix-like objects to be merged
#' @param sort.col Which column would you like the resulting data to be sorted
#'   on? Set to NULL to disable, in which case, rows corresponding to \code{x}
#'   will appear before those from \code{y}.
#' @return A data.frame with \code{nrow(x)} + \code{nrow(y)} rows, and
#'   \code{length(union(colnames(x), colnames(y)))} columns.
#' @author Mark Cowley, 11 April 2006
#' @seealso \code{\link{rbind}}
#' @keywords manip
#' @examples
#' 
#' a <- matrix(rnorm(25), 5, 5)
#' colnames(a) <- letters[1:5]
#' b <- matrix(rnorm(25), 5, 5)
#' colnames(b) <- letters[3:7]
#' rbind.smart(a, b)
#'
#' @export
rbind.smart <- function(x, y, sort.col=NULL) {
    if(ncol(x) == ncol(y) && identical(colnames(x), colnames(y)))
        return( rbind(x, y) )
    else {
        #
        # what are the possible colnames from both matrices?
        # usually, y has a subset of the columns of x.
        #
        COLNAMES <- union(colnames(x), colnames(y))

        #
        # keep colnames in the order that they were in "x".
        #
        tmp.order <- match(colnames(x), COLNAMES)
        COLNAMES <- COLNAMES[c(tmp.order, setdiff(1:length(COLNAMES), tmp.order))]

        #
        # in case the colclasses in x and y for same cols differ,
        # do y then x so that the classes in x take priority over the classes in y.
        #
        COLCLASSES <- rep("character", length(COLNAMES))
        names(COLCLASSES) <- COLNAMES
        COLCLASSES[colnames(y)] <- colclasses(y)
        COLCLASSES[colnames(x)] <- colclasses(x)

        # tmp function to resize a matrix and fill in with NA's
        resize <- function(x, COLNAMES, COLCLASSES) {
            tmp <- as.data.frame( matrix(NA, nrow(x), length(COLNAMES), dimnames=list(rownames(x), COLNAMES)) )
            tmp[,colnames(x)] <- x
            colclasses(tmp) <- COLCLASSES

            return(tmp)
        }

        if( length(setdiff(COLNAMES, colnames(x))) > 0 ) {
            x <- resize(x, COLNAMES, COLCLASSES)
        }
        if( length(setdiff(COLNAMES, colnames(y))) > 0 ) {
            y <- resize(y, COLNAMES, COLCLASSES)
        }

        #
        # now that x and y have same ncols:
        #
        res <- rbind(x, y)

        if( !is.null(sort.col) )
            res <- res[order(res[,sort.col]),]

        return( res )
    }
}
# CHANGELOG
# 2013-08-28: added explicit check for same colnames if ncol's are equal.
