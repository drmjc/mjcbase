\name{dir.same}
\alias{dir.same}
\title{Are 2 directories the same?}
\usage{
  dir.same(x, y)
}
\arguments{
  \item{x}{the path to at least 1 directory}

  \item{y}{the path to at least 1 directory}
}
\value{
  a logical, same length as \code{x}.
}
\description{
  This checks that 2 directories are the same size (as in
  physical size), and then prints out any discrepancies
  that it might find. This has been written with the intent
  that a user will look at the output.
}
\author{
  Mark Cowley, 2012-02-13
}

