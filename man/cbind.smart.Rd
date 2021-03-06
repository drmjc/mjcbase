% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbind.smart.R
\name{cbind.smart}
\alias{cbind.smart}
\title{A smarter cbind}
\usage{
cbind.smart(..., sort.col = NULL)
}
\arguments{
\item{sort.col}{Column to sort on. \dQuote{NULL} is ok.}

\item{\dots}{at least two matrix-like objects to be merged.}
}
\value{
a \code{data.frame} with all input objects cbound together, and with 1 row per unique
rowname across all the input matrix-like-objects, with \code{NA}'s inserted
where appropriate.
}
\description{
\code{cbind} only combines matrix-like objects when they contain identical rownames.
This function allows any number of matrix-like objects to be cbound, even if they
contain very few (or even no) rownames in common. The result will have 1 row per unique
rowname across all the input matrix-like-objects, and will have \code{NA}'s inserted
where appropriate.
The first argument will be in the top left of the result, followed by all columns of the
2nd argument, and any new rows added to the bottom, until the final obect is added in
the bottom right hand corner. Thus the result grows diagonally.
}
\examples{
a <- data.frame(matrix(rnorm(25), 5, 5))
dimnames(a) <- list(letters[1:5], LETTERS[1:5])
b <- data.frame(matrix(rnorm(25), 5, 5))
dimnames(b) <- list(letters[3:7], LETTERS[3:7])
c <- data.frame(matrix(rnorm(25), 5, 5))
dimnames(c) <- list(letters[11:15], LETTERS[11:15])
cbind.smart(a, b, c)
}
\author{
Mark Cowley, 2010-03-10
}
\seealso{
\code{\link{rbind.smart}}, \code{\link{cbind}}
}
\keyword{manip}

