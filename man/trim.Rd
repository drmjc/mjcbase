\name{trim}
\alias{trim}
\title{Trim the whitespace from front and back of the words in a vector.}
\usage{
  trim(x)
}
\arguments{
  \item{x}{a character vector}
}
\value{
  a character vector of trimmed text
}
\description{
  Trim the whitespace from front and back of the words in a
  vector.
}
\examples{
trim("   ABC")
# "ABC"
trim("DEF   ")
# "DEF"
trim("  ABC  ")
# "ABC"
}
\author{
  Mark Cowley, 2009-08-19
}

