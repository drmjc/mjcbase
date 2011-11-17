## Function to look for rows in a matrix like object that are identical in 2 columns of
## interest.
## NB The table does not have to be sorted in any particular order. If there are duplicate
## rows found, then the 1st 'hit' will be kept.
##
## usage:
##    rm.duplicate.rows(X)
##    rm.duplicate.rows(X, 1, 2)
##
## eg input
##     X[1:5,]
##         GenbankID   GeneID
##     1  AB010002.1    11438
##     2  AB016768.1   192199
##     3  AB017104.1    19691
##     4  AB036742.1   114660
##     5  AB036765.1    67112
##
## searches for all unique ID's in first column, then one at a time see if there
## are any duplicate entries for that GenBankID in the 2nd column.
##
## Mark Cowley, 23 Sept 2005
##
## updated on 7 Aug 2006 to be MUCH faster when x has many rows. probably slightly
## slower when x is fairly small but not by too much.
##
rm.duplicate.rows <- function(x, idx.tosearch=1, idx.tomatch=2, na.rm=T, issorted=F, verbose=T) {
	#
	# re-sort x to speed up searching, but remember the original order.
	#
	if( !issorted ) {
		x$MJCorder  <- 1:nrow(x)
		x <- x[order(x[,idx.tosearch], x[,idx.tomatch]),]
	}

	ids <- unique(x[,idx.tosearch])
	starts <- match(ids, x[,idx.tosearch])
	ends <- c(starts[2:length(starts)], nrow(x)+1) - 1
	#
	# which ids have > 1 rows?
	#
	duplicates <- which(ends != starts)
    if( length(duplicates) == 0 ) {
        x$MJCorder <- NULL
        return( x )
    }

	#
	# only consider these duplicated ids
	#
	ids <- ids[duplicates]
	starts <- starts[duplicates]
	ends <- ends[duplicates]

	if( verbose ) init.progress.meter2( length(ids) )
	rowstorm <- NULL
	for(i in 1:length(ids)) {
		if( verbose ) update.progress.meter2()

		idx <- starts[i]:ends[i]
		tmp <- x[idx, idx.tomatch]

		# strip the NA's from tmp since they're pesky!
		if( na.rm && any(is.na(tmp)) ) {
			nas <- which(is.na(tmp))
			# if they're all NA, then keep the first one soas to not
			# remove the id completely from the table.
			#
			if( length(nas) == length(tmp) ) {
				rowstorm <- c(rowstorm, idx[ nas[2:length(nas)] ])
				next
			}
			else if( length(nas) == (length(tmp) - 1) ) {
				rowstorm <- c(rowstorm, idx[nas])
				next
			}
			else {
				rowstorm <- c(rowstorm, idx[nas])
				tmp <- tmp[-nas]
				idx <- idx[-nas]
			}
		}
		#
		# for the remaining terms in tmp, only keep the
		# unique ones.
		#
		uc <- ucounts(tmp, issorted=T)
		idx.to.keep <- match(names(uc), tmp)
		if( length(idx.to.keep) != length(tmp) )
			rowstorm <- c(rowstorm, idx[-idx.to.keep])

# 		## if all elems are equal then they will all be == to the first element
# 		if( alleq(tmp) ) {
# 			rowstorm <- c(rowstorm, idx[2:length(idx)])
# 		}
# 		else { # they're either all different terms in tmp, or some different terms
# 			uc <- ucounts(tmp)
# 			#
# 			# uc is a named vector of counts
# 			# the value indicates the frequency of the term
# 			#
# 			if( all(uc==1) )
# 				next
# 			else {
# 				# keep the first record for each term, and the
# 				# extra records can then be signalled for removal.
# 				idx.to.keep <- match(names(uc), tmp)
# 				rowstorm <- c(rowstorm, idx[-idx.to.keep])
# 			}
# 		}
	}
	if( length(rowstorm) > 0 )
		x <- x[-rowstorm,]

	if( !issorted ) {
		x <- x[order(x$MJCorder), ]
		x$MJCorder <- NULL
	}

	return( x )
}
