#' Convert centimetres to inches
#' 
#' @param cm vector of measurements in cM
#' @return vector of measurements in inches
#' @author Mark Cowley, 2009-03-18
#' @export
cm2inch <- function(cm) {
	cm / 2.54
}
