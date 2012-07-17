# Convert a list to an adjacency matrix
#
# If x is a list of variably lengthed vectors, then return a table which has the
# unique values as the rownames, and the names(x) as the colnames, with either a 0 or 1
# depending on whether the entity in row i was found in the vector in column j. Sorted by
# decreasing frequency across the list of vectors (ie row 1 contains the word that was 
# found in the most number of vectors in list x)
#
# Parameters:
#	x: a named list, where each element is a character vector. If it's unnamed, names will be assigned
#
# Value:
#	a data.frame of 0 or 1, where rows correspond to unique words, columns represent each list element.
#
# Mark Cowley, 2009-10-29
#


#' Convert a list to an adjacency matrix
#' 
#' If x is a list of variably lengthed vectors, then return a table which has
#' the
#' unique values as the rownames, and the names(x) as the colnames, with either
#' a 0 or 1
#' depending on whether the entity in row i was found in the vector in column
#' j. Sorted by
#' decreasing frequency across the list of vectors (ie row 1 contains the word
#' that was
#' found in the most number of vectors in list x)
#' 
#' @param x a named list, where each element is a character vector. If it's
#'   unnamed, names will be assigned
#' @return a data.frame of 0 or 1, where rows correspond to unique words,
#'   columns represent each list element.
#' @author Mark Cowley, 2009-10-29
#' @export
list2adjmat <- function(x) {
	counts <- sort(table(unlist(x)), decreasing=TRUE)
	res <- as.data.frame(matrix(0,length(counts), length(x)), stringsAsFactors=FALSE)
	dimnames(res) <- list(names(counts), names(x))
	
	for(i in 1:length(x)) {
		idx <- match(x[[i]], names(counts))
		res[idx,i] <- 1
	}
	res
}
