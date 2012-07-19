#' replace the values in a table
#' 
#' replace the values in a table, at specific positions, referenced by the row name/index,
#' the column name/index.
#' 
#' Common usage scenario\cr
#' Given a spreadsheet of patients with mutated genes, you want to indicate in a square matrix
#' of genes x patients, which ones have a mutation. Thus a priori, you know which elements
#' you want to edit, but it's very fiddly using the "[", "]" nomenclature to indicate which
#' cells within the matrix to edit.
#'
#' @param tab a matrix or data.frame of values, to be edited
#' @param row a vector of row names or indices
#' @param col a vector of col names or indices
#' @param val a vector of values to insert into tab at the locations specified by row and col
#' 
#' @return a \code{matrix} or \code{data.frame} like tab, with those values specified by the
#' \code{row}, \code{col} tuples changed to the values specified by \code{val}.
#' 
#' @author Mark Cowley, 2011-09-20
#' @export
#' @examples
#' mat <- matrix(0, 5, 5)
#' replace.cells(mat, 1:5, 1:5, rep(NA, 5))
#' df <- as.data.frame(mat)
#' dimnames(df) <- list(letters[1:5], LETTERS[1:5])
#' replace.cells(df, c("a", "a", "e"), c("D", "B", "A"), 1:3)
#' 
replace.cells <- function(tab, row, col, val) {
	is.matrix(tab) || is.data.frame(tab) || stop("tab must be a matrix or data.frame")
	(length(row) == length(col)) && (length(row) == length(val)) || stop("row, col, val must be same length")
	all(!is.na(row)) || stop("row can't have any NA")
	all(!is.na(col)) || stop("col can't have any NA")
	(is.character(row) && is.data.frame(tab)) || is.numeric(row) || stop("tab must be a data.frame if is.character(row)")
	(is.character(col) && is.data.frame(tab)) || is.numeric(col) || stop("tab must be a data.frame if is.character(col)")
	(is.character(row) && all(row %in% rownames(tab))) ||
	(is.numeric(row) && min(row)>0 && max(row) <= nrow(tab)) || stop("values in row not found in tab")
	(is.character(col) && all(col %in% colnames(tab))) ||
	(is.numeric(col) && min(col)>0 && max(col) <= ncol(tab)) || stop("values in col not found in tab")
	
	for(i in 1:length(row)) {
		tab[row[i],col[i]] <- val[i]
	}
	
	tab
}
