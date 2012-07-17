\name{cbind.smart}
\alias{cbind.smart}
\title{A smarter cbind}
\usage{
  cbind.smart(x, y, sort.col = NULL)
}
\arguments{
  \item{x,y}{matrix-like objects to be merged}

  \item{sort.col}{Column to sort on. \dQuote{NULL} is ok.}
}
\value{
  A data.frame with \code{ncol(x)} + \code{ncol(y)} rows,
  and \code{length(union(rownames(x), rownames(y)))}
  columns.
}
\description{
  \code{cbind} 2 matrix-like-objects even if they have
  different numbers of rows.  It's very much like merge,
  but works nicely the same as \code{\link{rbind.smart}}.
}
\details{
  The resulting data.frame will have \code{ncol(x)} +
  \code{ncol(y)} rows, and \code{length(union(rownames(x),
  rownames(y)))} rows.

  If x and y contain the same rownames, then
  \code{cbind.smart} == \code{\link{cbind}}.

  If x and y contain partially overlapping rownames, then
  the result will be the union of all rownames, with NA's
  filled in where appropriate.

  If x and y contain no overlapping rownames, then the
  result will have x in top left and y in bottom right,
  filled in with NA's. as in: \preformatted{ x : X; y: Y
  cbind.smart(x, y) -> X NA NA Y } Naming rules: column
  classes from \code{x} take precedence over those from
  \code{y}, and the rownames of result will be all of the
  rownames from x, then the rownames from y that were not
  also in x at the end.
}
\examples{
a <- data.frame(matrix(rnorm(25), 5, 5))
dimnames(a) <- list(letters[1:5], LETTERS[1:5])
b <- data.frame(matrix(rnorm(25), 5, 5))
dimnames(a) <- list(letters[3:7], LETTERS[3:7])
cbind.smart(a, b)
}
\author{
  Mark Cowley, 2010-03-10
}
\seealso{
  \code{\link{rbind.smart}}, \code{\link{cbind}}
}
\keyword{manip}

