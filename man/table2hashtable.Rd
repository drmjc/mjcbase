\name{table2hashtable}
\alias{table2hashtable}
\title{Convert a table to a hashtable
Take a table with a \code{key.column}, and a \code{value.column}, and convert into a hashtable.}
\usage{
  table2hashtable(x, key.column = 1, value.column = 2,
    make.unique = TRUE, sort.method = sort)
}
\arguments{
  \item{x}{a \code{data.frame}}

  \item{key.column}{the column index or name where the keys
  are}

  \item{value.column}{the column index or name where the
  values are}

  \item{make.unique}{logical: keep redundant key->value
  pairs?}

  \item{sort.method}{the method to use to sort the values
  for each key}
}
\description{
  Convert a table to a hashtable Take a table with a
  \code{key.column}, and a \code{value.column}, and convert
  into a hashtable.
}
\note{
  Hashtable keys have to be <255 characters in length.
}
\author{
  Mark Cowley, 2009-07-13
}

