#' For all of the duplicates in x, return the indices for each of these
#' duplicates.
#' 
#' @section TODO: sort x so that you only need to do a local search for duplicates,
#' not a global search.
#' 
#' @param x a vector
#' @param is.sorted logical: ignored in this version
#' 
#' @return a list with each element corresponding to a duplicate entry in x.
#'   The value of each element is the numerical indices into x where this
#'   duplicate appeared. The name of each element is the duplicate name.
#' 
#' @author Mark Cowley, 20 March 2007
#' @export
duplicate.indices <- function(x, is.sorted=FALSE) {
	# warning("Code changed and hasn't been tested much (2009-07-16).")
	res <- vector2hashtable(x)
	res <- as.list(res)
	# res <- res[sapply(res, length)>1]
	return( res )
}
# duplicate.indices <- function(x, is.sorted=FALSE) {
#     if( is.sorted && !is.sorted(x) ) {
#         cat("x is not sorted; i'm sorting it for you now")
#         x <- sort(x)
#         cat(".\n")
#     }
#     if( is.sorted ) {
#         x.uniq <- unique(x)
#         res <- list()
# 
#         window <- max(ucounts(x)) # work out how far to look ahead
#         idx <- 1
#         for(i in 1:length(x.uniq)) {
#             #assign("tmp.x.uniq", x.uniq[i], pos=1)
#             tmp <- x[idx:min(c((idx+window-1), length(x)))] # get the next "window" elements; from 1:100 for eg.
#             indices <- which(tmp == x.uniq[i]) # which are a match to x.uniq[i]
# 
#             res[[i]] <- indices + idx - 1
#             idx <- indices[length(indices)] + idx
#         }
# 
#         names(res) <- x.uniq
#         return( res )
#     }
#     else {
#         ucounts <- ucounts(x)
#         ucounts <- ucounts[match(unique(x), names(ucounts))]
# 
#         dups <- ucounts[ucounts > 1]
#         res <- list()
# 
#         for(i in 1:length(dups)) {
#             res[[i]] <- which(x == names(dups)[i])
#         }
#         names(res) <- names(dups)
# 
#         return( res )
#     }
# }

#
# code to dynamically grow the size of tmp which doesn't quite work??
#
#         WINDOWsize <- max(max(ceiling((length(x) / length(x.uniq)) * 2), 100)
#         window <- WINDOWsize
#         idx <- 1
#         for(i in 1:length(x.uniq)) {
#             assign("tmp.x.uniq", x.uniq[i], pos=1)
#             tmp <- x[idx:min(c((idx+window-1), length(x)))] # from 1:100 for eg.
#             indices <- which(tmp == x.uniq[i]) # which are a match to x.uniq[i]
#             while(length(indices) == length(tmp)) {
#                 window <- window*2
#                 tmp <- x[idx:min(c((idx+window-1), length(x)))]
#                 indices <- which(tmp == x.uniq[i])
# #                 cat(".")
#             }
#             window <- WINDOWsize
