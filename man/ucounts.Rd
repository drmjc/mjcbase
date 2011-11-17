\name{ucounts}
\Rdversion{1.1}
\alias{ucounts}
\title{Count the number of occurrences of each word}
\description{This is a more flexible method than table, but in essence it returns the number of occurences of each word from a vector of words.  It's quite optimised for speed.}
\usage{
ucounts(x, return.unique = TRUE, issorted = FALSE, sort = TRUE)
}
\arguments{
  \item{x}{A character vector}
  \item{return.unique}{if TRUE, the numeric counts of the unique elements in x are returned (the names indicate the elements in x). NB the order of the returned elements is the same order that unique(x) returns.

if FALSE, then return the numeric counts of each element in x. Useful if you then want to work out which elements have count > 1 and quickly view those elements. NB, the order remains the same as x.
}
  \item{issorted}{If TRUE then we assume the data has been sorted, otherwise choose FALSE.}
  \item{sort}{If TRUE then the data will be sorted, otherwise choose FALSE.}
}
\details{
Possible usage scenarios include:
\preformatted{
  ucounts(c("a", "a", "b", "c", "c"), TRUE)
     a b c
     2 1 2
  ucounts(c("a", "a", "b", "c", "c"), FALSE)
     a a b c c
     2 2 1 2 2
  ucounts(c("a", "a", "b", "c", "c"), FALSE) > 1
        a     a     b     c     c
     TRUE  TRUE FALSE  TRUE  TRUE
}
}
\value{A named integer vector, where the names are from x}
\author{Mark Cowley}
\seealso{
\code{\link{table}}
}
\examples{
ucounts(c("a", "a", "b", "c", "c"), TRUE)
ucounts(c("a", "a", "b", "c", "c"), FALSE)
}
\keyword{manip}
