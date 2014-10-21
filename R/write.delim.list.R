#' Export a list of single tables into the same tab delimited file, seperated
#' by the name of each table.
#' 
#' If the tables all have the same column names, then the very first row in
#' the resulting file will be the column names, and the rest will not use
#' colnames
#' 
#' @param x a list of data.frames
#' @param f the output file name
#' @param \dots additional arguments to write.delim
#' @author Mark Cowley, 2008-10-01
#' @export
#' @importFrom excelIO write.delim
write.delim.list <- function(x, f, ...) {
	OUT <- file(f, "w")
	# write headers just once if they're all the same.
	allSAME <- TRUE
	for(i in 2:length(x)) {
		if( !alleq(colnames(x[[1]]) == colnames(x[[i]])) )
			allSAME <- FALSE
		if( !allSAME )
			break
	}
	
	if( allSAME ) {
		tmp <- paste(c("Name", colnames(x[[1]])), collapse="\t")
		writeLines(tmp, OUT)
		for(i in 1:length(x)) {
			write.delim(cbind(names(x)[i], x[[i]]), OUT, col.names=FALSE)
		}
	}
	else {
		for(i in 1:length(x)) {
			writeLines(names(x)[i], OUT)
			write.delim(x[[i]], OUT, col.names=TRUE, ...)
		}
	}
	close(OUT)
}
