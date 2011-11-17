## Function to convert a data frame of numeric values
## to a matrix of numeric values - for some reason
## x <- as.matrix(y) changes the numerics to characters?
##
## Mark Cowley, 21 Oct 2004
##
df2matrix.numeric <- function(x) {
  res <- matrix(NA, nrow(x), ncol(x))
  
 # if(nrow(x) > ncol(x)) {
    for(i in 1:ncol(x))
      res[,i] <- x[,i]
#  }
#  else {
#    for(i in 1:nrow(x))
#      res[i,] <- x[i,]
#  }

  rownames(res) <- rownames(x)
  colnames(res) <- colnames(x)

  res
}
