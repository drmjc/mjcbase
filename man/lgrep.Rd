\name{lgrep}
\alias{lgrep}
\title{grep for a pattern in a list of values.}
\usage{
  lgrep(pattern, x, ...)
}
\arguments{
  \item{pattern}{a character(1)}

  \item{x}{a list, usually where each element is a
  character vector.}

  \item{\dots}{arguments passed to \code{grepl}}
}
\value{
  returns the index into the list that contains your
  pattern.
}
\description{
  Why? grep normally tries to find a pattern in a vector of
  values; what if you have a list of vectors, and want to
  know in which vector does your value appear??
}
\author{
  Mark Cowley, 2009-10-16
}

