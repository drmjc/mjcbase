\name{collate.data.frame}
\alias{collate.data.frame}
\title{Collate the columns from two data.frames. ie produce a table that alternates
columns from the 2 tables}
\usage{
  collate.data.frame(x, y)
}
\arguments{
  \item{x}{two identically sized data.frames. Probably will
  work with matrices too.}

  \item{y}{two identically sized data.frames. Probably will
  work with matrices too.}
}
\value{
  a data.frame with ncol(x)*2
}
\description{
  Collate the columns from two data.frames. ie produce a
  table that alternates columns from the 2 tables
}
\author{
  Mark Cowley, 2009-12-16
}

