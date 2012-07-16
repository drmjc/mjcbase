#' all pairwise combinations from n
#' 
#' n choose 2 tells how many pairs can be made from n objects, \code{choose.pairs(n)}
#' gives the indices into a vector of objects of length \code{n}.
#' 
#' @param n a scalar, eg 10
#' 
#' @return 2 column \code{matrix}, with n choose 2 rows, and indices for each pair in
#' a row.
#' @author Mark Cowley, 28 Jan 2005
#' @export
#' @examples
#' choose.pairs(4)
choose.pairs <- function(n) {
	if(length(n) == 1) {
		n <- seq(1, n)
	}
	npairs <- choose(length(n), 2)
	res <- data.frame(matrix(NA, npairs, 2))
	row <- 1
	for(i in 1:(length(n)-1)) {
		for(j in (i+1):length(n)) {
			res[row,] <- c(n[i], n[j])
			row <- row + 1
		}
	}
	colnames(res) <- c("i","j")
	return(res)
}

#' all unique combinations of 3 elements from n
#' 
#' n choose 3 tells how many triplets can be made from n objects,
#' \code{choose.trios(n)} gives the indices into a vector of objects of length n.
#' 
#' @param n a scalar, eg 10
#' @return 3 column \code{matrix}, with n choose 3 rows, and indices for each triplet
#' in a row.
#' 
#' @author Mark Cowley, 28 Jav 2005
#' @export
#' @examples
#' choose.trios(5)
choose.trios <- function(n) {
	ntrios <- choose(n, 3)
	res <- data.frame(matrix(NA, ntrios, 3))
	row <- 1
	for(i in 1:(n-2)) {
		for(j in (i+1):(n-1)) {
			for(k in (j+1):n) {
				res[row,] <- c(i,j,k)
				print(i,j,k)
				row <- row + 1
			}
		}
	}
	colnames(res) <- c("i","j","k")
	return(res)

}
