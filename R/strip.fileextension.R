#' Strip the file extension
#' Strip the three letter file extension (such as dQuote(.txt), or
#' dQuote(.csv)) and the dot from a character vector of filenames
#' 
#' The code looks to be more generic than just three characters, but I haven't
#' checked this in much detail yet.
#' 
#' @param files A character vector of file names.
#' @return A character vector with the file extensions, including the dot
#'   removed.
#' @author Mark Cowley, 10 Nov 2006
#' @keywords manip
#' @examples
#' 
#' a <- c("file1.txt", "file1.updated.txt")
#' strip.fileextension(a)
#' 
#' @export
strip.fileextension <- function(files) {
    files <- sub("\\.[^.]{2,}$", "", files)
    return(files)
}
