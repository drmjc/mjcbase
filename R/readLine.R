

## related to readLine and strsplit
readLine <- function(file, split=" +", ok=F) {

    tmp <- readLines(file, n=1, ok=T)
    if( length(tmp) == 0 )
        return(tmp)
    else
        return( strsplit( trim(tmp), split )[[1]] )
}

skipLine <- function(file) {
    tmp <- readLines(file, n=1, ok=T)
}
