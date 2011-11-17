## Correlate 2 sdp strings
## either as strcor("DBBBBBDBBDDDBBDDBDBDDDDDBBDDBBBB","BDBDBBBBDBDDBDDBBBBBBDBDBDBBBBDH")
## or as 2 same lengthed vectors eg strcor(genotypes[1,], genotypes[2,])
##
## Mark Cowley, 31 August 2005
##
strcor <- function(x,y) {
    if(length(x)==1)
        x <- paste(x, collapse="")
    if(length(y)==1)
        y <- paste(y, collapse="")

    return( cor(as.numeric(as.factor(x)), as.numeric(as.factor(y))) )
}
