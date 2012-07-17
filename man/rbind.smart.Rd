\name{rbind.smart}
\alias{rbind.smart}
\title{A smarter rbind
\code{rbind} 2 matrix-like-objects even if they have different numbers of
columns.  It's a bit like \code{merge()} but via rowbindings, not
colbindings. It produces a result which as \code{union(colnames(a),
colnames(b))}, and fills in missing data with \code{NA}. See details.}
\usage{
  rbind.smart(x, y, sort.col = NULL)
}
\arguments{
  \item{x,y}{matrix-like objects to be merged}

  \item{sort.col}{Which column would you like the resulting
  data to be sorted on? Set to NULL to disable, in which
  case, rows corresponding to \code{x} will appear before
  those from \code{y}.}
}
\value{
  A data.frame with \code{nrow(x)} + \code{nrow(y)} rows,
  and \code{length(union(colnames(x), colnames(y)))}
  columns.
}
\description{
  The resulting data.frame will have \code{nrow(x)} +
  \code{nrow(y)} rows, and \code{length(union(colnames(x),
  colnames(y)))} columns.
}
\details{
  If x and y contain the same colnames, then
  \code{rbind.smart} == \code{rbind}.

  If x and y contain partially overlapping colnames, then
  the result will be the union of all colnames, with NA's
  filled in where appropriate.

  If x and y contain no overlapping colnames, then the
  result will have x in top left and y in bottom right,
  filled in with NA's. as in: \preformatted{ x : X; y: Y
  rbind.smart(x, y) -> X NA NA Y } Naming rules: column
  classes from \code{x} take precedence over those from
  \code{y}, and the colnames of result will be all of the
  colnames from x, then the colnames from y that were not
  also in x at the end.
}
\examples{
a <- matrix(rnorm(25), 5, 5)
colnames(a) <- letters[1:5]
b <- matrix(rnorm(25), 5, 5)
colnames(b) <- letters[3:7]
rbind.smart(a, b)
}
\author{
  Mark Cowley, 11 April 2006
}
\seealso{
  \code{\link{rbind}}
}
\keyword{manip}

