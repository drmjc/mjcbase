\name{threshold.distance.matrix}
\alias{threshold.distance.matrix}
\title{threshold a distance matrix}
\usage{
  threshold.distance.matrix(d, max.distance)
}
\arguments{
  \item{d}{an object of class \code{dist}.}

  \item{max.distance}{a single numeric distance threshold,
  eg \code{0.9}}
}
\value{
  an object of class \code{dist}, with rows/columns removed
  for entities that failed the thresholding.
}
\description{
  Code to take a distance matrix, and threshold out
  entitites that are too distant from all other entities
}
\author{
  Mark Cowley, 2009-09-03
}

