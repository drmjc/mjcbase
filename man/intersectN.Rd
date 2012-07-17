\name{intersectN}
\alias{intersectN}
\title{Intersection of multiple vectors
This function extends \code{\link{intersect}} which only accepts 2 vectors,
to allow many vectors.}
\usage{
  intersectN(...)
}
\arguments{
  \item{\dots}{The names of various vectors that you wish
  to intersect.}
}
\value{
  A vector of elements that are found in all input vectors.
}
\description{
  This method is nicely optimised at each iteration so that
  it compares the smallest lists first, and stops if there
  are no elements in the intersection.
}
\examples{
a <- letters[1:15]
b <- letters[5:20]
c <- letters[10:25]
intersectN(a, b, c)
}
\author{
  Mark Cowley
}
\seealso{
  \code{\link{intersect}}
}
\keyword{array}
\keyword{logic}

