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
  This function has more capabilities than path.expand, in
  that relative paths (. and ./ and ../) are also expanded.
}
\examples{
\dontrun{
get.full.path("~", trailing.slash=FALSE)
get.full.path("~", trailing.slash=TRUE)
get.full.path("./")
get.full.path("..")
}
}
\author{
  Mark Cowley
}

