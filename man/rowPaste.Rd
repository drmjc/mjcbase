\name{rowPaste}
\alias{rowPaste}
\title{Convert rows in a table to a character vector, where each column contains
words which will be seperated by 'sep'}
\usage{
  rowPaste(x, sep = ", ")
}
\arguments{
  \item{x}{a matrix-like object}

  \item{sep}{the separator character}
}
\value{
  a character vector of row elements pasted together,
  seperated by sep
}
\description{
  Convert rows in a table to a character vector, where each
  column contains words which will be seperated by 'sep'
}
\author{
  Mark Cowley, 2009-07-29
}

