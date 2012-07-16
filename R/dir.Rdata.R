#' dir listing of R data objects
#'
#' @param path the path
#' 
#' @return a character vector listing of all R data files within \code{path}
#' @author Mark Cowley
#' @export
dir.Rdata <- function(path=".") {
	dir(path=path, pattern=".R[dD]ata$|.R[dD]ata.gz$|Rda|RDa|Rda.gz|RDa.gz")
}
