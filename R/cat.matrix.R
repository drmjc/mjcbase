# Print a matrix to stdout.
#
# Mark Cowley
# 2010-01-12: edited to print row and column names.
cat.matrix <- function (x, row.names=TRUE, col.names=TRUE, pad=TRUE, sep=" ") {
    if(row.names && !is.null(rownames(x))) x <- rownames2col(x, 1, " ")
    if(col.names && !is.null(colnames(x))) x <- colnames2row(x, 1, " ")
	if( pad ) {
		for(i in 1:ncol(x)) {
			x[,i] <- as.character(x[,i])
			w <- max(nchar(x[,i]))
			cmd <- paste("%",w,"s", sep="")
			x[,i] <- sprintf(cmd, x[,i])
		}
	}
    for (i in 1:nrow(x)) {
        for (j in 1:ncol(x)) {
            cat(x[i, j], sep, sep = "")
        }
        cat("\n")
    }
}

test.cat.matrix <- function() {
	m <- matrix(1:9,3,3)
	m.r <- m; rownames(m.r) <- LETTERS[1:3]
	m.c <- m; colnames(m.c) <- letters[1:3]
	m.rc <- m.c; rownames(m.rc) <- LETTERS[1:3]

	df <- as.data.frame(m)
	df.r <- df; rownames(df.r) <- LETTERS[1:3]
	df.c <- df; colnames(df.c) <- letters[1:3]
	df.rc <- df.c; rownames(df.rc) <- LETTERS[1:3]
	
	for(rn in c(TRUE, FALSE)) {
		for(cn in c(TRUE, FALSE)) {
			cat(sprintf("Testing rownames %d, colnames %d.\n", rn, cn))
			cat("matrices.\n")
			cat("no dimnames.\n")
			cat.matrix(m, row.names=rn, col.names=cn)
			cat("only rownames.\n")
			cat.matrix(m.r, row.names=rn, col.names=cn)
			cat("only colnames.\n")
			cat.matrix(m.c, row.names=rn, col.names=cn)
			cat("has dimnames.\n")
			cat.matrix(m.rc, row.names=rn, col.names=cn)

			cat("data.frames.\n")
			cat("no dimnames.\n")
			cat.matrix(df, row.names=rn, col.names=cn)
			cat("only rownames.\n")
			cat.matrix(df.r, row.names=rn, col.names=cn)
			cat("only colnames.\n")
			cat.matrix(df.c, row.names=rn, col.names=cn)
			cat("has dimnames.\n")
			cat.matrix(df.rc, row.names=rn, col.names=cn)
		}
	}
}