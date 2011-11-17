is.alnum <- function(x) {
  return( length(grep("[^a-zA-Z0-9_]", as.character(x))) == 0 )
}

is.alpha <- function(x) {
  return( length( grep("[^A-Za-z]", as.character(x) ) ) == 0 )
}

to.alnum <- function(x) {
  for( j in 1:(length(x)) ) {
    if(!is.alnum(x[j])) {
      v <- to.char.array(x[j])
      for(i in 1:length(v)) {
        if(!is.alnum(v[i]))
          x[j] <- paste(substr(x[j],1,i-1),"_", substring(x[j], i+1), sep="")
      }
    }
  }
  x
}
