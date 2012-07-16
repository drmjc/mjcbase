\name{recycle}
\alias{recycle}
\title{Variable recycler
Recycle the elements in \code{x} to make a vector that has length \code{N}.}
\usage{
  recycle(x, N)
}
\arguments{
  \item{x}{A vector of length >= 1}

  \item{N}{The desired length of the result. Should be >=
  2.}
}
\value{
  A vector of length N, with elements made up by repeating
  x as many times as necessary.
}
\description{
  Typical usages are: \preformatted{ recycle(1:8, 17)
  recycle(c("red", "blue", "green"), 7) } If
  \code{length(x) == 1} then this is equivalent to
  \code{rep(x)}. \code{N} does not have to be an even
  number of multiples of \code{length(x)}.
}
\examples{
recycle(1:8, 17)
recycle(c("red", "blue", "green"), 7)
}
\author{
  Mark Cowley, 9 Nov 2005
}
\seealso{
  \code{\link{rep}}
}
\keyword{manip}

