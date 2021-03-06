% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.pairs.R
\name{choose.trios}
\alias{choose.trios}
\title{all unique combinations of 3 elements from n}
\usage{
choose.trios(n)
}
\arguments{
\item{n}{a scalar, eg 10}
}
\value{
3 column \code{matrix}, with n choose 3 rows, and indices for each triplet
in a row.
}
\description{
n choose 3 tells how many triplets can be made from n objects,
\code{choose.trios(n)} gives the indices into a vector of objects of length n.
}
\examples{
choose.trios(5)
}
\author{
Mark Cowley, 28 Jav 2005
}

