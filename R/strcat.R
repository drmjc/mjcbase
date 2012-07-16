#' concatenate a bunch of strings
#' 
#' @param \dots character vectors to be concatenated
#' @author Mark Cowley, 11 April 2006
#' @export
#' @examples
#' strcat(letters)
#' # [1] "abcdefghijklmnopqrstuvwxyz"
strcat <- function(...) {
	paste0(unlist( list(...) ))
}
