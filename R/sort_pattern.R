#' pattern sorting a vector
#' 
#' sort a character vector based on the presence of multiple patterns. \code{\link{sort}}
#' only gives you alphanumeric sorting, and sometimes you want to sort on patterns within
#' the words. Eg given a character vector of filepaths containing label suffixes, or structured
#' components, you may want to reorder so that the keywords are all grouped. See examples.
#' 
#' @section TODO:
#' Implement \code{decreasing} and \code{na.last} like \code{\link[base]{sort}}
#' 
#' @param x a character vector, or a vector which can be converted to one.
#' @param patterns a character vector of patterns. see \code{\link{grep}}.
#' 
#' @return a character vector representation of x, with the elements reodered. 
#'  If no elements in \code{x} match any pattern then these will be at the end.
#' The order of elements within those that match a pattern 
#'  are the same as in \code{x}. The patterns are ordered, such that once an element has 
#' matched a pattern, it will not be re-sorted.
#' 
#' @author Mark Cowley, 2013-09-25
#' @export
#' @examples
#' a <- c("simple-AAA-100", "simple-AAA-200", "simple-AAA-300", "xyz-BBB-100", "xyz-BBB-200", "xyz-BBB-300")
#' sort_pattern(a, c("300", "200", "100"))
#' sort_pattern(a, c("xyz", "simple"))
#' sort_pattern(a, c("xyz", "AAA"))
#' sort_pattern(a, c("xyz", "AAA", "300")) # the last term has no effect
#' 
#' #
#' # real world example.
#' #
#' a <- c("CCDS_CDS_hg19_uniq", "CCDS_exons_hg19_uniq", "CDSgencodeV16_uniq", 
#' "knownGene_CDS_hg19_uniq", "knownGene_exons_hg19_uniq", "knownGene_UTR_hg19_uniq", 
#' "LncRNAgencodeV16_uniq", "miRNAgencodeV16pluslength_uniq", "PsuedoGenesgencodeV16_uniq", 
#' "refGene_CDS_hg19_uniq", "refGene_exons_hg19_uniq", "refGene_UTR_hg19_uniq", 
#' "SnoRNAgencodeV16plusLength_uniq", "SnRNAgencodeV16plusLength_uniq", 
#' "start_codengencodeV16_uniq", "stop_codengencodeV16_uniq", "UnprocessedPsudoGenegencodeV16_uniq", 
#' "UTRgencodeV16_uniq")
#' sort_pattern(a, c("gencode", "exons", "CDS", "UTR"))
#' 
sort_pattern <- function(x, patterns, ...) {
	x <- as.character(x)
	
	res <- NULL
	for(pattern in patterns) {
		if( length(x) == 0 ) break
		vals <- grep(pattern, x, value=T)
		x <- grep(pattern, x, value=T, invert=T)
		res <- c(res, vals)
	}
	if( length(x) > 0 ) res <- c(res, x)
	res
}
