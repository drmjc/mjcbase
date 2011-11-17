quote.vector <- function(x) {
  for(i in 1:length(x))
    x[i] <- paste("'", x[i], "'", sep="")

  return(x)
}
