\name{unduplicate}
\alias{unduplicate}
\title{unduplicate elements in a vector}
\usage{
  unduplicate(x, sep = ".")
}
\arguments{
  \item{x}{a character vector}

  \item{sep}{the seperator between
  \code{<original><sep><integer>}}
}
\value{
  a character vector, \code{length(x)}, where all elements
  are unique
}
\description{
  Similar to \code{\link{make.unique}}, ensure that all
  elements in \code{x} are unique. Note that duplicate can
  mean any number of repetitions, not just 2. This differs
  from \code{\link{make.unique}} in that \emph{all}
  instances of a duplicated element are tagged with a
  numeric suffix, as opposed to the 2:n elements that get
  tagged by \code{\link{make.unique}}. see examples
}
\examples{
x <- c("a","a","a","b","b")
unduplicate(x)
# [1] "a.1" "a.2" "a.3" "b.1" "b.2"
# compared to make.unique where the first 'a' and 'b' are unchanged.
make.unique(x)
# [1] "a"   "a.1" "a.2" "b"   "b.1"
unduplicate(letters[1:6])
# [1] "a" "b" "c" "d" "e" "f"
unduplicate(c(x,NA, NA, NA))
# [1] "a.1" "a.2" "a.3" "b.1" "b.2" NA    NA    NA
}
\author{
  Mark Cowley, 2012-05-02
}
\seealso{
  \code{\link{make.unique}}
}

