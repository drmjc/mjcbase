#' Add columns to a \code{data.frame} or \code{matrix}
#' 
#' Alter the columns within a \code{data.frame} or \code{matrix}, to \code{columns}. Note this
#' can have a side effect of reordering, adding new, and excluding existing columns,
#' such that the final result will the same columns in the same order as specified
#' by \code{columns}.
#'
#' @param x a \code{data.frame}, \code{ExpressionSet}, more to come?
#' @param columns a chracter vector of column names
#' @return a \code{data.frame} or same class as \code{class(x)}
#' @author Mark Cowley, 2012-09-04
#' @S3method add_columns default
#' @S3method add_columns data.frame
#' @S3method add_columns matrix
#' @rdname add_columns-methods
#' @examples
#' if (require(datasets)) {
#' # data.frame method:
#' data(iris)
#' add_columns(head(iris), c(colnames(iris), "newColumn"))
#' #   Sepal.Length Sepal.Width Petal.Length Petal.Width Species newColumn
#' # 1          5.1         3.5          1.4         0.2  setosa        NA
#' # 2          4.9         3.0          1.4         0.2  setosa        NA
#' # 3          4.7         3.2          1.3         0.2  setosa        NA
#' # 4          4.6         3.1          1.5         0.2  setosa        NA
#' # 5          5.0         3.6          1.4         0.2  setosa        NA
#' # 6          5.4         3.9          1.7         0.4  setosa        NA
#'
#' # matrix method 
#' mat <- as.matrix(iris[1:5,1:4])
#' add_columns(mat, c("Sepal.Length", "Sepal.Width", "newColumn", "Petal.Length"))
#' #   Sepal.Length Sepal.Width newColumn Petal.Length
#' # 1          5.1         3.5        NA          1.4
#' # 2          4.9         3.0        NA          1.4
#' # 3          4.7         3.2        NA          1.3
#' # 4          4.6         3.1        NA          1.5
#' # 5          5.0         3.6        NA          1.4
#' }
#' 
#' @seealso \code{\link{merge_tsv}}
add_columns <- function(x, columns) UseMethod("add_columns")

add_columns.default <- function(x, columns) {
	stop("unsupported data type for x")
}

add_columns.data.frame <- function(x, columns) {
	# is(x, "data.frame") || stop("x should be a data.frame")
	
	if( all(columns %in% colnames(x)) ) x
	new.columns <- setdiff(columns, colnames(x))
	new.data <- data.frame(matrix(NA, nrow(x), length(new.columns)))
	dimnames(new.data) <- list(row.names(x), new.columns)
	res <- data.frame(x, new.data)
	res <- res[, columns]
	
	res
}

add_columns.matrix <- function(x, columns) {
	cat("dispatching add_columns.matrix\n")
	
	if( all(columns %in% colnames(x)) ) x
	new.columns <- setdiff(columns, colnames(x))
	new.data <- matrix(x[1,1], nrow(x), length(new.columns)) # get the data type right
	dimnames(new.data) <- list(row.names(x), new.columns)
	new.data <- colapply(new.data, function(x) rep(NA, length(x)))
	res <- cbind(x, new.data)
	res <- res[, match(columns, colnames(res))]
	
	res
}

# add_columns.ExpressionSet <- function(x, columns) {
# 	
# 	if( all(columns %in% sampleNames(x)) ) x
# 	new.columns <- setdiff(columns, sampleNames(x))
# 	new.data <- matrix(NA, nrow(x), length(new.columns))
# 	dimnames(new.data) <- list(row.names(x), new.columns)
# 	new.data <- new("ExpressionSet", assayData=assayDataNew(exprs=new.data))
# 	res <- c(x, new.data)
# 	res <- res[, columns]
# 	
# 	res
# }
# debug(add_columns.ExpressionSet)
# b <- add_columns.ExpressionSet(bcl2.seq.TD, a)
