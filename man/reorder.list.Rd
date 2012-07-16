\name{reorder.list}
\alias{reorder}
\alias{reorder.list}
\title{Reorder the elements in a list}
\usage{
  reorder(x, order, ...)
}
\arguments{
  \item{x}{a \code{list}}

  \item{order}{vector of indices}

  \item{\dots}{unused}
}
\value{
  a \code{list}
}
\description{
  Reorder the elements in a list
}
\examples{
l <- list(a=1,b=2,c=3)
reorder.list(l, c(2,1,3))
reorder(l, c(2,1,3))
}
\author{
  Mark Cowley, 2011-11-17
}

