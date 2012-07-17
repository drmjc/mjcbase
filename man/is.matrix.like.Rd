\name{is.matrix.like}
\alias{is.matrix.like}
\title{is x a \code{matrix} or \code{data.frame}?}
\usage{
  is.matrix.like(x)
}
\arguments{
  \item{x}{an R object}
}
\value{
  logical: \code{TRUE} if \code{x} is a \code{matrix} or
  \code{data.frame}; \code{FALSE} otherwise
}
\description{
  is x a \code{matrix} or \code{data.frame}?
}
\examples{
m <- matrix(1:25,5)
is.matrix.like(m)
df <- as.data.frame(m)
is.matrix.like(df)
is.matrix.like(letters)
# FALSE
}
\author{
  Mark Cowley, 2011-07-18
}

