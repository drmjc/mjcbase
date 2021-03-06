% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowMax.R
\name{colMax}
\alias{colMax}
\title{column-wise maximum}
\usage{
colMax(x, na.rm = TRUE)
}
\arguments{
\item{x}{a \code{matrix} or \code{data.frame}}

\item{na.rm}{logical: remove NA's?}
}
\value{
a vector of maxima, 1 per column in \code{x}
}
\description{
column-wise maximum
}
\examples{
m <- matrix(1:25,5,5)
colMax(m)
}
\author{
Mark Cowley, 2011-10-20
}

