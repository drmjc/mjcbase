\name{symmetricise}
\alias{symmetricise}
\title{Make a pair symmetrical
Convert a vector of 2 values into symmetrical ones. This is mostly used
during plotting to make xlim or ylim symmetrical about zero.}
\usage{
  symmetricise(x)
}
\arguments{
  \item{x}{a numeric vector of length 2. See
  \code{\link{range}}}
}
\value{
  A numeric vector of length 2
}
\description{
  Typical usage scenarios are: \preformatted{ symmetricise(
  c(-0.6, 1.1) ) -> c(-1.1, 1.1) symmetricise( c(0.6, 1.1)
  ) -> c(-1.1, 1.1) symmetricise( c(-3.4, 1.1) ) -> c(-3.4,
  3.4) }
}
\examples{
x <- rnorm(100)
r <- range(x)
r2 <- symmetricise(r)
plot(density(x), xlim=r2)
}
\author{
  Mark Cowley, 5 April 2006
}
\seealso{
  \code{\link{range}}
}
\keyword{manip}

