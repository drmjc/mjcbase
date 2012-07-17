#' gzip a file.
#' @param from the file name to be gzipped
#' @return none. side effect of gzipping a file.
#' @author Mark Cowley, 27 May 2005
#' @export
file.gzip <- function(from) {
    system( paste(sep="", "cd ", dirname(from), " && gzip -f ", basename(from) ) )
}


#' gunzip a file.
#' 
#' @param from the absolute or relative to ~ path of the file to unzip
#' @param to instead of unzipping '<from>.gz' to <from>, unzip to <to>
#' @return invisibly returns the filename of the unzipped file.
#' @author Mark Cowley, 28 Oct 2005
#' @export
file.gunzip <- function(from, to=NULL) {
    if( is.null(to) ) {
        system( paste(sep="", "cd ", dirname(from), " && gunzip -f ", basename(from) ) )
        to <- gsub("\\.gz$", "", from)
    }
    else {
        system( paste("cat", from, "| gunzip >", to) )
    }
    #
    # invisibly return the to dir.
    #
    invisible( to )
}


#' Is the file a gzip file?
#' @param f the path to a file name
#' @return \code{TRUE} if \code{f} is a gzipped file, \code{FALSE} otherwise
#' @author Mark Cowley, 2009-01-13
#' @export
file.isgzip <- function(f) {
	str_right(f, 2) %in% c("gz", "Gz", "GZ")
}
