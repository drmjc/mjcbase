\name{get.full.path}
\alias{get.full.path}
\title{Expand a file path to an absolute path}
\usage{
  get.full.path(path, trailing.slash = TRUE)
}
\arguments{
  \item{path}{A character vector of paths to files or
  directories}

  \item{trailing.slash}{logical. if TRUE, and path is a
  directory, include a trailing \dQuote{/}}
}
\value{
  A character vector containing the full paths to files or
  directories given by \dQuote{file}
}
\description{
  This function has more capabilities than
  \code{path.expand}, in that relative paths (. and ./ and
  ../ and implicit ./) are also expanded, and more than
  \code{normalizePath}, in that files with an implicit ./
  are also expanded relative to the current working dir,
  and files that don't currently exist can also expanded
  (see examples).
}
\examples{
\dontrun{
path.expand("~")
normalizePath("~")
get.full.path("~", trailing.slash=FALSE)
get.full.path("~", trailing.slash=TRUE)

path.expand("./")
normalizePath("./")
get.full.path("./")

path.expand("../")
normalizePath("../")
get.full.path("..")

#
# benefits over other approaches
# - file doesn't have to exist already
get.full.path("sgsdf.zip")
# [1] "/Users/marcow/src/R/mjcbase/R/sgsdf.zip"
path.expand("sgsdf.zip")
# [1] "sgsdf.zip"
normalizePath("sgsdf.zip")
# [1] "sgsdf.zip"
}
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{normalizePath}}
}

