#' Determines the difference between two time points generated from calling
#' date()
#' 
#' @param t1 a date string eg "Wed Oct 20 14 03:12 2004"
#' @param t2 as for \code{t1}
#' @param units one of \dQuote{sec}, \dQuote{min} or \dQuote{hr}
#' 
#' @return amount oftime between t2 - t1.	
#' 
#' @section TODO:
#' Make the function work for when t2 and t1 are on different days.
#' 
#' @note check out lubridate package.
#' 
#' @author Mark Cowley, 20 Oct 2004
#' @export
time_diff <- function(t1, t2, units="min") {
	nelems <- length(unlist(strsplit(t1, " ")))
	
	t1 <- unlist(strsplit(t1, " "))[nelems-1]
	t1 <- as.numeric( unlist(strsplit(t1, ":")) )
	t2 <-	unlist(strsplit(t2, " "))[nelems-1]
	t2 <- as.numeric( unlist(strsplit(t2, ":")) )

	if(units == "sec") {
		time <- 60*60*(t2[1] - t1[1]) + 60*(t2[2] - t1[2]) + (t2[3] - t1[3])
	}
	else if(units == "min") {
		time <- 60*(t2[1] - t1[1]) + (t2[2] - t1[2]) + (t2[3] - t1[3])/60
	}
	else if(units == "hr") {
		time <- (t2[1] - t1[1]) + (t2[2] - t1[2])/60 + (t2[3] - t1[3])/3600
	}
	else {
		warning("units must be 1 of \"sec\", \"min\", \"hr\".\n")
		time <- 0
	}

	return(time)
}
