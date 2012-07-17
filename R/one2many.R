#' convert one2many and many2one
#' 
#' @description convert back and forth between one 2 many data, ie one
#' key vs multiple values
#' 
#' @details \code{n2o}: convert many-to-one ie convert a list of vectors into a char vector, with
#' elements in each input vector separated by sep. Useful for displaying one-to-many data in a single column
#' of a table.
#' 
#' @param x a list of elements, each element being a vector with  >=1 value
#' @param sep the field seperator
#' 
#' @return \code{n2o}: a character vector of values, where elements that had >1 value are separated
#' by sQuote{sep}
#' @author Mark Cowley, 15 March 2007
#' @export
#' @rdname one2many
#' @examples
#' # n2o
#' x <- list(104009, c(71772, 231691)) 
#' n2o(x) 
#' # [1] "104009", "71772 /// 231691"
n2o <- function(x, sep=" /// ") {
	if(!is.list(x)) x <- list(x)
	res <- sapply(x, function(x) paste(as.character(x), collapse=sep))
	if( any(is.na(x)) )
		res[is.na(x)] <- NA
	res
}


#' @details \code{o2n}: convert one-to-many ie convert a char vector of entries separated by sep
#' into a list of vectors. Useful for displaying one-to-many data in a single column
#' of a table.
#' @examples
#' # o2n
#' y <- c("104009", "71772 /// 231691")
#' o2n(y)
#' # [[1]]
#' # [1] "104009"
#' # [[2]]
#' # [1] "71772" "231691"
#' @export
#' @rdname one2many
o2n <- function(x, sep=" /// ") {
	strsplit(x, sep)
}

#' @details \code{o2nu}: convert one-to-many ie convert a char vector of entries separated by sep
#' into a list of vectors, and make elements unique. Useful for displaying one-to-many data in a single column
#' of a table.
#' 
#' @return \code{o2nu}: a list of vectors, where each vector has just one value
#' @examples
#' y <- c("104009", "71772 /// 71772")
#' o2n(y)
#' # [[1]]
#' # [1] "104009"
#' # [[2]]
#' # [1] "71772" "231691"
#' o2nu(y)
#' # [[1]]
#' # [1] "104009"
#' # [[2]]
#' # [1] "71772"
#' 
#' @export
#' @rdname one2many
o2nu <- function(x, sep=" /// ") {
	lapply(o2n(x, sep=sep), unique)
}


#' @details \code{map2o}: convert two columns into a o2n object.
#' 
#' @param map a matrix-like object with >= 2 columns
#' @param idx1 which column contains the "keys"
#' @param idx2 which column contains the "values"
#' @param make.unique logical: if there are duplicate values for one key, keep the
#'   duplicates, or remove them entirely?
#' @param sort.method howto sort the keys. the default is the "sort" function.
#' 
#' @return \code{map2o}: a 2 column \code{data.frame} with the keys in the first column and the
#'   values as n2o in the 2nd column. (ie separated by " /// ").
#' 
#' @section TODO: make it quicker if the map is sorted by the keys.
#' @export
#' @rdname one2many
#' 
#' @examples
#' # map2o
#' map <- cbind( c("a", "a", "a", "b", "c"),
#'               c("1", "2", "2", "1", "4")) 
#'
#' map2o(map, 1, 2, TRUE)
#' # "a", "1 /// 2"
#' # "b", "1"
#' # "c", "4"
#' map2o(map, 1, 2, FALSE)
#' # "a", "1 /// 2 /// 2"
#' # "b", "1"
#' # "c", "4"
#' map2o(map, 1, 2, TRUE, rev)
#' # "a" , "2 /// 1"
#' # "b" , "1"
#' # "c" , "4"
#' map2o(map, 1, 2, FALSE, rev)
#' # "a", "2 /// 2 /// 1"
#' # "b", "1"
#' # "c", "4"
map2o <- function(map, idx1=1, idx2=2, make.unique=TRUE, sort.method=sort) {
	map <- map[order(map[,idx1]), c(idx1, idx2)]
	keys <- unique(map[,idx1])

	res <- as.data.frame(matrix(c(keys, rep(NA, length(keys))), ncol=2, byrow=FALSE))
	colclasses(res) <- c("character")

	# for each key, remember which rows in map that it is found.
	key.row.indices <- duplicate.indices(map[,1], is.sorted=TRUE)
	res[,2] <-
		sapply(key.row.indices,
			function(x) {
				tmp <- map[x,2]
				if( !is.null(sort.method) )
					tmp <- sort.method( tmp )
				if( make.unique )
					tmp <- unique(tmp)
				return( n2o(tmp) )
			})
	if( is.null(colnames(map)) )
		colnames(res) <- c("key", "values")
	else
		colnames(res) <- colnames(map)

	return( res )
}


#' @details \code{map2list}: Convert 2 columns in a n:n map into a 1:n list, such that each unique
#' element in map[,idx1] is a list element, and all of the things that are in
#' the idx2'th column.
#' 
#' @return \code{map2list}: A named \code{list} of keys to values
#' @export
#' @rdname one2many
map2list <- function(map, idx1=1, idx2=2, make.unique=TRUE, sort.method=sort) {
	# Version 4
	res <- table2hashtable(map, idx1, idx2, make.unique=make.unique, sort.method=sort.method)
	res <- as.list(res)
	return(res)

	# V3
	# # for each key, remember which rows in map that it is found.
	# rowmap <- vector2hashtable(map[, idx1])
	# 
	# .func <- function(x, values, make.unique, sort.method) {
	# 	x <- values[x]
	# 	if( !is.null(sort.method) )
	# 		x <- sort.method( x )
	# 	if( make.unique )
	# 		x <- unique(x)
	# 	x
	# }
	# res <- eapply(rowmap, .func, map[, idx2], make.unique, sort.method)
	# res <- as.list(res)
	# return( res )

	# V2
	# keys <- names(ls(env=rowmap))
	# res <- list()
	# 
	# for(key in keys) {
	# 	indices <- lookUp(rowmap, key)
	# 	tmp <- map[indices, 2]
	# 	if( !is.null(sort.method) )
	# 		tmp <- sort.method( tmp )
	# 	if( make.unique )
	# 		tmp <- unique(tmp)
	# 	res[[key]] <- tmp
	# }
	# return( res )
	
	# V1
	# res <-
	# 	lapply(key.row.indices,
	# 		function(x) {
	# 			tmp <- map[x,2]
	# 			if( !is.null(sort.method) )
	# 				tmp <- sort.method( tmp )
	# 			if( make.unique )
	# 				tmp <- unique(tmp)
	# 			return( tmp )
	# 		})
	# 
	# names( res ) <- keys
	# return( res )
}


#' @details \code{map2env}: convert a map to an environment. This is more memory 
#' efficient than running \code{\link{map2list}} then \code{\link{list2env}}.
#' Convert 2 columns in a n:n map into a 1:n list, such that
#' each unique element in map[,idx1] is a list element, and
#' all of the things that are in the idx2'th column.
#' 
#' @return \code{map2env}: An environment
#' @export
#' @rdname one2many
map2env <- function(map, idx1=1, idx2=2, make.unique=TRUE, sort.method=sort) {
	env <- new.env(hash=TRUE)

	#
	# sort the map by the idx1 column, and only keep the idx1, idx2 columns
	# in that order.
	#
	map <- map[order(map[,idx1]), c(idx1, idx2)]
	keys <- unique(map[,1])

	# for each key, remember which rows in map that it is found.
	key.row.indices <- duplicate.indices(map[,1], is.sorted=TRUE)
	lapply(key.row.indices,
			function(indices) {
				tmp <- map[indices,2] # get all the indices
				if( !is.null(sort.method) )
					tmp <- sort.method( tmp )
				if( make.unique )
					tmp <- sunique(tmp)
				assign(map[indices[1],1], tmp, env)
			})

	return ( env)
}

#' @details \code{list2env}: convert a named \code{list} of keys to values into a hashed environment
#' 
#' @param envir an environment, or \code{NULL}
#' @return \code{list2env}: a hashed environment
#' @export
#' @rdname one2many
list2env <- function(x, envir=NULL) {
	if( is.null(envir) )
		envir <- new.env(hash=TRUE)

	for(i in 1:length(x) ) {
		assign(names(x)[i], x[[i]], envir=envir)
	}

	invisible( envir )
}

#' @details \code{list2map}: Convert a list, where element names = key, and element vectors are the
#' values into a 2 column map.
#' 
#' @return \code{list2map}: a 2 column \code{data.frame}, with these elements:
#' \item{keys}{search key}
#' \item{value}{mapped value}
#' @export
#' @rdname one2many
list2map <- function(x) {
	if( any(duplicated(names(x))) )
		stop("names of x must be unique.")

	n <- sapply(x, length)
	if( any(n==0) ) {
		x <- subset(x, n>0)
		n <- sapply(x, length)
	}
	res <- data.frame(key=rep(names(x), times=n), value=unlist(x))
	res
}
