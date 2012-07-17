\name{lookUp}
\alias{lookUp}
\title{generic key lookUp
Generic function to look up keys in a variety of data objects.}
\usage{
  lookUp(keys, data, key.column = "row.names",
    data.columns = NULL, mode = c("all", "first"))
}
\arguments{
  \item{keys}{a vector of search keys (ie words)}

  \item{data}{a vector, \code{matrix} or \code{data.frame}}

  \item{key.column}{the key column if data is a
  \code{matrix} or \code{data.frame}. default is to use the
  \dQuote{row.names}}

  \item{data.columns}{which columns to search in if
  \code{data} is tabular? default=\code{NULL}, which
  searches all columns}

  \item{mode}{find \dQuote{all} matches, or the
  \dQuote{first} match (faster)?}
}
\value{
  the values corresponding to the keys
}
\description{
  It works on vector or list data, where the names of each
  element are the key. It works on vectors with no names,
  in which case the indices of the keys are returned It
  works on tabular data, where the key is identified in
  \code{key.column} and the columns returned are specified
  by \code{data.columns}.
}
\author{
  Mark Cowley, 2009-07-09
}

