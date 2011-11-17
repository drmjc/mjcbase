

zeros <- function(nrow, ncol) {
    matrix(0, nrow, ncol)
}


ones <- function(nrow, ncol) {
    matrix(1, nrow, ncol)
}


normrnd <- function(mean=0, sd=1) {
    if( is.matrix.like(mean) ) {
        if( is.matrix.like(sd) && dim(sd) != dim(mean) ) {
            stop("mean and sd are not the same sized matrices\n")
        }
        else if( !is.matrix.like(sd) && length(sd) == nrow(mean) ) {
            sd <- matrix(rep(sd, ncol(mean)), nrow(mean), ncol(mean), byrow=F)
        }
        else if( !is.matrix.like(sd) && length(sd) == ncol(mean) ) {
            sd <- matrix(rep(sd, nrow(mean)), nrow(mean), ncol(mean), byrow=T)
        }
        else if( length(sd) == 1 ) {
            sd <- matrix(rep(sd, prod(dim(mean))), nrow(mean), ncol(mean), byrow=T)
        }
        else {
            stop("mean and sd are incompatible in their dimensions")
        }
    }
    else if( is.matrix.like(sd) ) {
        if( length(mean) == nrow(sd) ) {
            mean <- matrix(rep(mean, ncol(sd)), nrow(sd), ncol(sd), byrow=F)
        }
        else if( length(mean) == ncol(sd) ) {
            mean <- matrix(rep(mean, nrow(sd)), nrow(sd), ncol(sd), byrow=T)
        }
        else if( length(mean) == 1 ) {
            mean <- matrix(rep(mean, prod(dim(sd))), nrow(sd), ncol(sd), byrow=T)
        }
        else {
            stop("mean and sd are incompatible in their dimensions")
        }
    }
    else {
        if( length(mean) == length(sd) ) {
            ; ## do nothing
        }
        if( length(mean) < length(sd) && length(sd) %% length(mean) == 0 ) {
            mean <- rep(mean, length(sd)/length(mean) )
        }
        else if( length(mean) > length(sd) && length(mean) %% length(sd) == 0 ) {
            sd <- rep(sd, length(mean)/length(sd) )
        }
        else {
            stop("lengths of mean and sd are incompatible\n")
        }
    }

    if( is.matrix.like(mean) ) {
        res <- rnorm(prod(dim(mean)), mean=as.numeric(mean), sd=as.numeric(sd))
        res <- matrix(res, nrow(mean), ncol(mean), byrow=FALSE)
    }
    else {
        res <- rnorm(length(mean), mean=mean, sd=sd)
    }

    return( res )
}


corceff <- function(x) {
    cor(x)
}