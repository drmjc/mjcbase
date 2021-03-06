% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trunc.R
\name{trunc}
\alias{trunc}
\title{truncate the values in a vector}
\usage{
trunc(x, limits)
}
\arguments{
\item{x}{a numeric vector}

\item{limits}{a numeric(2) indicating the min and max value. If either 
is \code{NA}, then data will not be truncated at the min or max.}
}
\value{
a numeric vector, same length as \code{x}, with values no
smaller than limits[1] and no larger than limits[2]
}
\description{
truncate the values in a vector
}
\examples{
x <- rnorm(20, 0, 2)
trunc(x, c(-1,1))
trunc(x, c(-1,NA))
trunc(x, c(NA,1))
}
\author{
Mark Cowley, 2011-11-29
}

