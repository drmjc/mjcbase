\name{choose.pairs}
\alias{choose.pairs}
\title{all pairwise combinations from n}
\usage{
  choose.pairs(n)
}
\arguments{
  \item{n}{a scalar, eg 10}
}
\value{
  2 column \code{matrix}, with n choose 2 rows, and indices
  for each pair in a row.
}
\description{
  n choose 2 tells how many pairs can be made from n
  objects, \code{choose.pairs(n)} gives the indices into a
  vector of objects of length \code{n}.
}
\examples{
choose.pairs(4)
}
\author{
  Mark Cowley, 28 Jan 2005
}

