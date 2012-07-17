\name{tocsv}
\alias{tocsv}
\title{Convert a vector x into a comma separated character(1).}
\usage{
  tocsv(x, sep = ", ")
}
\arguments{
  \item{x}{a vector}

  \item{sep}{the character to join the values with}
}
\value{
  a character(1) with each element of \code{x} joined
  together using \code{sep}
}
\description{
  Useful for pasting a list of entries in to a batch search
  engine. You can change comma separator by changing the
  value of\code{sep}.
}
\author{
  Mark Cowley, 15 May 2006
}
\seealso{
  \code{\link{dump}} \code{\link{dput}}
}

