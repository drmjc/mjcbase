## Determine the correlation of a set of genes. If you want to know the
## pairwise correlation of a vector of genes, supply a character vector.
## If you want to know how correlated N genes are to a specific gene, you
## can supply the vector of N genes (genes=c(A,B,C,D)) and the reference gene
## (ref=X). You can supply a list of char vectors and an equally lengthed
## vector of reference genes, and the results will be iteratively returned.
## ref is recylced if genes is a list and length(genes) > length(ref).
##
## eg:
##     cor.3tissues(c("NM_008084", "AK000001"), expression$M)
##     cor.3tissues(c("NM_008084", "AK000001"), ref="AJ00001", expression$M)
##     cor.3tissues(list(c("NM_008084", "AK000001"),
##                       c("NM_000001", "NM_00002"),
##                  ref=c("AJ00001", "AJ00002"),
##                  expression$M)
##
## Parameters:
##     genes: 1) A character vector
##            2) A list of character vectors
##     ref: 1) NULL -- all pairwise combinations of genes in the vectors in genes.
##          2) character: correlation between this reference gene and the genes in
##             the vectors
##          3) character vector with length == length(genes). Vector N in genes will
##             be correlated to reference gene N, N is 1:length(genes)
##          4) character vector with length < length(genes): ref is recycled, then
##             (3) above will hold true.
##
## Value:
##     in all cases, a list of 3 objects names B, K, L. The objects depend on
##     genes and ref:
##     genes = (1) and ref = (1) then matrix of pairwise combos of correlations
##       within genes is returned
##     genes = (1) and ref = (2|3) then vector of correlations of the ref gene
##       vs all of the genes in genes is returned
##     genes = (2) and:
##       ref = (1): a list of N pairwise cor matrices of the genes in each
##         element in genes[[N]]
##       ref = (2|3|4): a list of N cor vectors from correlating genes[[N]] to
##         ref[N], where ref has been recylced if necessary
##
## Mark Cowley, 29 March 2006
##
cor.3tissues <- function(genes, ref=names(genes), expression=expressed755.qpt, method="pearson") {
    if ( !is.list(genes) ) {
        res <- cor.3tissues( list(genes), ref=ref, expression=expression, method=method )
        res <- lapply(res, function(x) x[[1]] )
        return( res )
    }


    if( !is.null(ref) ) {
        #
        # Then we want to know the correlation of the reference gene to all of
        # the others in the vector to which that ref gene references
        #
        if(length(ref) < length(genes)) {
            ref <- recycle(ref, length(genes))
        }


        cor <- list(B=list(), K=list(), L=list())
        for(i in 1:length(genes)) {
            cor$B[[i]] <- rep(0, length(genes[[i]]))
            cor$K[[i]] <- rep(0, length(genes[[i]]))
            cor$L[[i]] <- rep(0, length(genes[[i]]))

            REF <- ref[i]

            for(j in 1:length(genes[[i]])) {
                gene <- genes[[i]][j]

                cor$B[[i]][j] <- cor(as.numeric(expression$B[REF, ]), as.numeric(expression$B[gene, ]), method=method)
                cor$K[[i]][j] <- cor(as.numeric(expression$K[REF, ]), as.numeric(expression$K[gene, ]), method=method)
                cor$L[[i]][j] <- cor(as.numeric(expression$L[REF, ]), as.numeric(expression$L[gene, ]), method=method)
            }
        }
    }
    else {
        #
        # We want to know the pairwise correlations of the vector of genes.
        #
        cor <- list(B=list(), K=list(), L=list())
        for(i in 1:length(genes)) {
            cor$B[[i]] <- cor(t(expression$B[genes[[i]], ]), method=method)
            cor$K[[i]] <- cor(t(expression$K[genes[[i]], ]), method=method)
            cor$L[[i]] <- cor(t(expression$L[genes[[i]], ]), method=method)
        }
    }


    return( cor )
}




plot.cor.3tissues <- function(cor, ref=cor(t(expressed755.qpt$BKL)), main="", ...) {

    if( class(ref) != "density" )
        ref <- density( ref )

    if( is.vector(cor$B) )
        cor <- lapply(cor, function(x) list(x))

    auto.mfrow(length(cor$B), setup=T)
    par(mar=c(2,3,2,1))


    for(i in 1:length(cor$B)) {
        plot( density(cor$B[[i]]), col=4, xlim=c(-1, 1), ylim=c(0, 3), main="", xlab="cor", ...)
        title(main=paste(main, " N =", length(cor$B[[i]])))
        abline(v=0, lty=2, col=6)

        lines( density(cor$K[[i]]), col=3)
        lines( density(cor$L[[i]]), col=2)

        if( !is.null(ref) ) {
            lines( ref, col=1 )
            legend("topright", legend=c("B","K","L","ref\n755"), col=4:1, lty=1, bty="n")
        }
        else {
            legend("topright", legend=c("B","K","L"), col=4:2, lty=1, bty="n")
        }
    }

    auto.mfrow(length(cor$B), setup=F)



}