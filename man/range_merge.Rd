\name{range_merge}
\alias{range_merge}
\title{merge a set of numbers into a range.}
\usage{
  range_merge(x, gaplength = 0)
}
\arguments{
  \item{x}{a numeric \code{vector}}

  \item{gaplength}{allow a gap within a run. default=0}
}
\description{
  Useful for working out what is the minimal information to
  describe a series of numbers
}
\note{
  This is almost certainly replaceble using code from
  IRanges package, and Rle obects.
}
\examples{
x <- c(1, 2, 3, 4, 5, 9)
range_merge(x)
#   from to
# 1    1  5
# 2    9  9
range_merge(x, gaplength=3)
#   from to
# 1    1  9
range_merge(x, gaplength=2)
#   from to
# 1    1  5
# 2    9  9
}
\author{
  Mark Cowley
}

