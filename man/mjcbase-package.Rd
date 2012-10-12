\docType{package}
\name{mjcbase-package}
\alias{mjcbase}
\alias{mjcbase-package}
\title{mjcbase}
\description{
  A collection of utility code for Mark J Cowley. This
  description file only scratches the surface. The goal of
  this package is to be useful, yet very lightweight with
  as few depedencies as possible. Note none of this code is
  guaranteed to run on Windows, but should work on OSX and
  linux. This is because I often use \code{system()} calls
  to speed some things up.
}
\section{data I/O utilties}{
  \code{\link{pbpaste}} and \code{\link{pbcopy}} for
  copying objects to and from the system clipboard.
  \code{\link{read.table.wide}} and
  \code{\link{read.table.fast}} for importing very wide
  tables, and susbets of columns from very long tables,
  respectively.\cr \code{\link{write.delim}} and
  \code{\link{write.delim.list}}. see also my excelIO
  package for reading & writing Excel files.
  \code{\link{readLine}}, \code{\link{skipLine}} for
  parsing text files.
}

\section{file utilities}{
  \code{\link{file.gzip}}, \code{\link{zip}},
  \code{\link{zipdir}}, \code{\link{file.issymlink}},
  \code{\link{file.move}}, \code{\link{file.numlines}}
  \code{\link{is.dir}}, \code{\link{is.url}},
  \code{\link{dir.Rdata}}, \code{\link{dir.remove}},
  \code{\link{dir.size}}, \code{\link{crossplatform.unzip}}
}

\section{data utilities}{
  \code{\link{alleq}} \code{\link{even}}
  \code{\link{odd}}\cr \code{\link{collate.data.frame}},
  \code{\link{interleave.columns}}\cr
  \code{\link{rename.column}}, \code{\link{col2rownames}},
  \code{\link{row2colnames}}, \code{\link{colnames2row}},
  \code{\link{rownames2col}} for moving columns and rows in
  and out of \code{data.frame}'s. \code{\link{tocsv}} and
  \code{\link{uncsv}} to converting vectors to/from csv
  data\cr

  \tabular{ll}{ Package: \tab mjcbase\cr Type: \tab
  Package\cr Version: \tab 1.0.6\cr Date: \tab
  2012-07-31\cr License: \tab GPL-2\cr LazyLoad: \tab
  yes\cr }
}
\author{
  Mark Cowley Maintainer: Mark Cowley
  \email{m.cowley@garvan.org.au}
}
\keyword{package}

