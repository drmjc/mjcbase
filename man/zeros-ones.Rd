\name{zeros}
\alias{ones}
\alias{zeros}
\title{zeros ones}
\usage{
  zeros(nrow, ncol)

  ones(nrow, ncol)
}
\arguments{
  \item{nrow}{number of rows}

  \item{ncol}{number of columns}
}
\value{
  a \code{matrix} of \code{dim(nrow, ncol)} of all 0's or
  1's
}
\description{
  create a matrix of 0's or 1's, like the Matlab functions
  of the same name.
}
\examples{
zeros(5,3)
ones(5,3)
}
\author{
  Mark Cowley
}

