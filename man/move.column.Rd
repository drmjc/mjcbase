\name{move.column}
\alias{move.column}
\title{Reorder a column, or block of columns from within a data.frame.}
\usage{
  move.column(x, cols2move, at)
}
\arguments{
  \item{x}{a data.frame}

  \item{cols2move}{The column(s) that you want to move. Can
  be a vector of length >= 1 of either column names, or
  indices. If more than one are specified, and they are not
  contiguous, they will be properly extracted from the
  data.frame, but brought together into a contiguous block,
  then inserted into result before the specified column}

  \item{at}{which column do you want the new columns to be
  inserted BEFORE. if <= 1, then move to fron, if >
  ncol(x), or at=NULL, then move the columns to end.}
}
\description{
  The colnames must not be null, and must be unique
}
\author{
  Mark Cowley, 2009-01-20
}

