#' @export
setClassUnion("logicalORmissing", c("logical", "missing"))

#' stratify
#' 
#' stratify x into 0,1, based on a percentile or a threshold
#'
#' This was designed to convert continuous data (eg gene expression levels)
#' and discretize into low/high as 0/1, respectively. These data can be a simple
#' vector, or 2D \code{matrix} or \code{data.frame}, or a \code{list} where each element
#' is a \code{numeric(1)}. You can stratify that data using either a \code{percentile},
#' eg 0.25 or 0.5 for the 25th and 50th percentile; or a threshold, eg 6.5.
#' 
#' If your data is 1D, then you should supply either a single percentile or threshold. 
#' If you have 2D data, then you can provide
#' a single percentile or threshold, or 1 percentile or threshold for each row in x. these 
#' values are recycled if necessary to match \code{nrow(x)}.
#' 
#' If you supply both \code{percentile} and \code{threshold}, then the \code{percentile} will be used.
#' If you supply neither, then \code{percentile=0.5} is used.
#' 
#' @param x a numeric \code{vector}, \code{matrix}, \code{data.frame} or \code{list}
#' @param percentile numeric: The percentile at which to cut the data, in the range (0,1). Eg 
#' 75th percentile = 0.75
#' If \code{x} is 2D, then you can specify 1 percentile for all rows, or 1 percentile per row in \code{x}.
#' @param threshold numeric: The numeric threshold at which to cut the data. This should
#' not be \code{NULL}, or \code{NA}, and if specified, should be within the range of (min(x), max(x)).
#' If \code{x} is 2D, then you can specify 1 threshold for all rows, or 1 threshold per row in \code{x}.
#' \code{percentile} overrides \code{threshold} if both are specified.
#' @return a numeric \code{vector}, \code{matrix}, \code{data.frame} or \code{list} of 0/1 values, where 0 means below the
#'  cutpoint, and 1 means >= the cutpoint.
#' 
#' @author Mark Cowley, 2011-09-01
#' @exportMethod stratify
#' @rdname stratify-methods
#' @docType methods
#' 
setGeneric(
	"stratify",
	function(x, percentile, threshold, as.factor=FALSE) {
		standardGeneric("stratify")
	}
)

#' @rdname stratify-methods
#' @aliases stratify,numeric,numeric,missing,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("numeric", "numeric", "missing", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		percentile > 0 && percentile < 1 || stop("percentile must be in (0,1)")
		threshold <- quantile(x, probs=percentile)
		cat(sprintf("%.2f\n", threshold))
		stratify(x, threshold=threshold, as.factor=as.factor)
	}
)

#' @rdname stratify-methods
#' @aliases stratify,numeric,missing,numeric,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("numeric", "missing", "numeric", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		threshold > min(x, na.rm=TRUE) && threshold < max(x, na.rm=TRUE) || stop("threshold must be in (min(x),max(x))")

		res <- as.numeric( x >= threshold )
		if( as.factor ) {
			res <- factor(res, levels=c(0,1))
			levels(res) <- c("low", "high")
		}
		res
	}
)

#' @rdname stratify-methods
#' @aliases stratify,numeric,numeric,numeric,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("numeric", "numeric", "numeric", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		warning("you specified both percentile and threshold. ignoring threshold")
		stratify(x, percentile, as.factor=as.factor)
	}
)


#' @rdname stratify-methods
#' @aliases stratify,matrix,numeric,missing,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("matrix", "numeric", "missing", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		# res <- apply(x, 1, quantile, probs=percentile)
		# res
		
		length(percentile) %in% c(1, nrow(x)) || stop("length(percentile) != 1 || nrow(x)")
		
		percentile <- recycle(percentile, nrow(x))
		
		res <- list()
		for(i in 1:nrow(x)) {
			res[[i]] <- stratify(x[i,,drop=TRUE], percentile=percentile, as.factor=as.factor)
		}
		names(res) <- sprintf("%s (%d%%)", rownames(x), round(percentile*100))

		res
	}
)

#' @rdname stratify-methods
#' @aliases stratify,matrix,missing,numeric,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("matrix", "missing", "numeric", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		
		length(threshold) %in% c(1, nrow(x)) || stop("length(threshold) != 1 || nrow(x)")
		threshold  <- recycle(threshold , nrow(x))
		
		res <- list()
		for(i in 1:nrow(x)) {
			res[[i]] <- stratify(x[i,], threshold=threshold[i], as.factor=as.factor)
		}
		names(res) <- sprintf("%s (thresh=%.2f)", rownames(x), threshold)

		res
	}
)


#' @rdname stratify-methods
#' @aliases stratify,matrix,numeric,numeric,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("matrix", "numeric", "numeric", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		warning("you specified both percentile and threshold. ignoring threshold")
		stratify(x, percentile, as.factor=as.factor)
	}
)

################################################################################
# coerce different objects into matrices.
################################################################################

#' @rdname stratify-methods
#' @aliases stratify,data.frame,ANY,ANY,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("data.frame", "ANY", "ANY", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		isNumeric <- colclasses(x) == "numeric"
		mat <- as.matrix(x[,isNumeric])
		stratify(mat, percentile, threshold, as.factor)
	}
)


#' @rdname stratify-methods
#' @aliases stratify,ExpressionSet,ANY,ANY,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("ExpressionSet", "ANY", "ANY", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		# stratify(exprs(x), percentile, threshold, as.factor=as.factor)
		x <- exprs(x)
		callNextMethod()
	}
)


#' @rdname stratify-methods
#' @aliases stratify,list,ANY,ANY,logicalORmissing-method
setMethod(
	"stratify",
	signature=signature("list", "ANY", "ANY", "logicalORmissing"),
	function(x, percentile, threshold, as.factor) {
		all(sapply(x, class)=="numeric") || stop("if x is a list, it must be a list of numeric(1)'s")
		all(sapply(x, length)==1) || stop("if x is a list, it must be a list of numeric(1)'s")
		res <- lapply(x, function(y) stratify(y, percentile, threshold, as.factor))
		names(res) <- names(x)
		res
	}
)


#' @rdname stratify-methods
#' @aliases stratify,ANY,missing,missing,missing-method
setMethod(
	"stratify",
	signature=signature("ANY", "missing", "missing", "missing"),
	function(x, percentile, threshold, as.factor) {
		percentile <- 0.5
		stratify(x, percentile=percentile, as.factor)
	}
)
