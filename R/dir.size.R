#' directory size
#' This determines the size of a directory by adding the size of the individual files
#' 
#' @param x a vector of directory path(s)
#' @param unit for displaying object size: "bytes", "KB", "MB", or
#'        first letter. default="bytes"
#' @return a vector of sizes in \code{units}
#' @author Mark Cowley, 2012-02-13
#' @export
dir.size <- function(x, unit=c("bytes", "KB", "MB", "GB")) {
	unit <- substring(tolower(unit[1]), 1, 1)
	res <- rep(NA, length(x))
	for(i in 1:length(x)) {
		files <- list.files(x[i], all.files=TRUE, recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
		fi <- file.info(files)
		res[i] <- sum(fi$size)
	}
	names(res) <- x
	
	res <- switch(unit,
		b=res,
		k=round(res/1024,1),
		m=round(res/1024/1024,1),
		g=round(res/1024/1024/1024,1)
	)
	
	res
}

#' Are 2 directories the same?
#' 
#' This checks that 2 directories are the same size (as in physical size),
#' and then prints out any discrepancies that it might find.
#' This has been written with the intent that a user will look at the output.
#' 
#' @param x the path to at least 1 directory
#' @param y the path to at least 1 directory
#' @return a logical, same length as \code{x}.
#' @author Mark Cowley, 2012-02-13
#' @export
dir.same <- function(x, y) {
	length(x) == length(y) || stop("x and y must be the same length")
	is.dir(x) && is.dir(y) || stop("x and y must be directories")
	
	res <- rep(FALSE, length(x))
	
	for(i in 1:length(x)) {
		res[i] <- is.dir(x[i]) && is.dir(y[i]) && (round(dir.size(x[i]),3) == round(dir.size(y[i]),3))
		if( !res[i] ) {
			cat(sprintf("%s vs %s\n", x[i], y[i]))
			cat(sprintf("%d (%f GB) vs %d (%f GB)\n", 
				length(x.files <- list.files(x[i], all.files=TRUE, recursive=TRUE, full.names=FALSE, include.dirs=TRUE)), 
				dir.size(x[i], "GB"), 
				length(y.files <- list.files(y[i], all.files=TRUE, recursive=TRUE, full.names=FALSE, include.dirs=TRUE)), 
				dir.size(y[i], "GB")
			))
			# print.venn(x.files, y.files)
			cat(sprintf("These files only in '%s':\n%s\n", x[i], paste(setdiff(x.files, y.files), collapse="\n")))
			cat(sprintf("These files only in '%s':\n%s\n", y[i], paste(setdiff(y.files, x.files), collapse="\n")))
		}
		else {
			cat(sprintf("%s vs %s -- SAME\n", x[i], y[i]))			
		}
	}

	res
}

#' Are the subdirs that are named to same actually identical
#'
#' Given 2 dirs that may share a number of subdirs, which at least by
#' name look to be similar, this function checks whether those directories
#' actually are the same in terms of file size.
#' 
#' Why bother? If your data is on local and server, then invariably this data
#' will be out of sync, and one will be newer than the other. it's often very 
#' difficult to determine this just by looking at the folder names.
#' 
#' @param x character(1) the path to a directory
#' @param y character(1) the path to a directory
#' @return logical(N), per subdir found in both dirs.
#' @author Mark Cowley, 2012-02-13
#' @export
subdirs.same <- function(x, y) {
	length(x) == length(y) && length(x) == 1 || stop("x and y must be the same length")
	is.dir(x) && is.dir(y) || stop("x and y must be directories")
	
	subdirs <- intersect(dir(x), dir(y))
	subdirs <- subdirs[is.dir(file.path(x,subdirs))]
	cat(sprintf("Found %d subdirectories in common\n", length(subdirs)))
	if( length(subdirs) > 0 ) {
		res <- dir.same(file.path(x,subdirs), file.path(y,subdirs))
		names(res) <- file.path(x,subdirs)
	}
	
	res
}

