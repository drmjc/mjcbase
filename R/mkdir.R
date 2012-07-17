#' Manipulaton of Directories and File Permissions
#' 
#' This is a wrapper around \code{base::\link[base]{dir.create}}, which
#' creates directories.
#' 
#' @inheritParams base::dir.create
#' @return
#' mkdir returns invisibly a logical vector
#'   indicating if the operation succeeded for each of the files
#'   attempted.  Using a missing value for a path name will always be
#'   regarded as a failure.  \code{dir.create} indicates failure if the
#'   directory already exists.  If \code{showWarnings = TRUE}, \code{dir.create}
#'   will give a warning for an unexpected failure (e.g. not for a
#'   missing value nor for an already existing component for \code{recursive
#'   = TRUE}).
#' @author Ross Ihaka, Brian Ripley
mkdir <- function(path, showWarnings = TRUE, recursive = FALSE, mode = "0777") {
	dir.create(path=path, showWarnings=showWarnings, recursive=recursive, mode=mode)
}
