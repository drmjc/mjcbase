setGeneric("print.venn", 
  function(x,y) standardGeneric("print.venn")
)

setMethod("print.venn",
	signature=signature(x="character", y="character"),
	function(x, y) {
	    all <- intersect(x, y)
	    x <- setdiff(x, all)
	    y <- setdiff(y, all)
	    tmp <- c(
	        length(x),
	        length(all),
	        length(y) )

	    res <- matrix(c(tmp, sum(tmp)), nrow=1)
	    res <- rbind(as.character(res), round( res[1,] / res[1,4] * 100, 2))
	    colnames(res) <- c("  A ~ B", "  A & B", "  B ~ A", "  A | B")
	    rownames(res) <- c(" N", " %")
	    res <- as.data.frame(res, stringsAsFactors=FALSE)

	    res
	}
)
setMethod("print.venn",
	signature=signature(x="ANY", y="ANY"),
	function(x, y) {
		print.venn(as.character(x), as.character(y))
	}
)
setMethod("print.venn",
	signature=signature(x="environment",y="environment"),
	function(x,y){
		print.venn(Lkeys(x), Lkeys(y))
	}
)
