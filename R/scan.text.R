#' scan.text
#' 
#' Read data into a vector or list from the console or file.
#'
#' @details This differs from \code{base::\link[base]{scan}}, in that
#' it automatically imports contents as \code{character}
#' 
#' @inheritParams base::scan
#' @param \dots arguments passed to \code{\link{scan}}
#' 
#' @return a \code{character vector}
#' 
#' @author Mark Cowley, 2012-07-06
#' @export
scan.text <- function(file, blank.lines.skip=FALSE, strip.white=TRUE, quiet=TRUE, ...) { #"
    scan( file, what=character(1), sep="\n",
          strip.white=strip.white, quiet=quiet, blank.lines.skip=blank.lines.skip, ... )
}
