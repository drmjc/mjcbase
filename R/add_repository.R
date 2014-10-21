#' point \R to an additional package repository
#'
#' @param path the path to a repository. note this should be the root level, where 'src/contrib' exists.
#' @param name the repository name.
#' @return nothing. sets the 'repos' option.
#' @author Mark Cowley, 2013-05-20
#' @export
#' @examples
#' \dontrun{
#' # for packages that live in ~/src/contrib, specify this:
#' add_repository("file:///Users/marcow", "local")
#' getOption("repos")
#' 
#' # <libdir>/src/contrib exists
#' add_repository(paste0("file://", normalizePath(libdir), "/"), "local")
#' getOption("repos")
#' 
#' # real example; add the NuGO repository
#' add_repository("http://nmg-r.bioinformatics.nl/Custom_CDF_DB_V16.0.0/", "NUGO")
#' }
add_repository <- function(path, name) {
	!missing(path) || stop("Must specify a path to repository")
	!missing(name) || stop("Must specify a short name for the repository")
	r <- getOption("repos")
	if( name %in% names(r) ) warning("overwriting old value of repository")
	r[name] <- path
	options(repos=r)
}
