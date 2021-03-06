% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unionN.R
\name{unionN}
\alias{unionN}
\title{Union of 2 or more vectors}
\usage{
unionN(...)
}
\arguments{
\item{\dots}{at least 2 vectors to be unioned}
}
\value{
A vector containing the union of all the input vectors.
}
\description{
This function extends the \code{\link[base]{union}} function to allow more than 2
vectors to be unioned. In practice, we do not use union at all, but a more
optimised algorithm.
}
\details{
Values are returned in the same order that they are first detected when\cr
iterating though \dots
}
\examples{

a <- letters[1:15]
b <- letters[5:20]
c <- letters[10:25]
unionN(a, b, c)

}
\author{
Mark Cowley
}
\seealso{
\code{\link{intersectN}}, \code{\link{union}}
}
\keyword{array}
\keyword{logic}

