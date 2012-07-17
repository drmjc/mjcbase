\name{strip.fileextension}
\alias{strip.fileextension}
\title{Strip the file extension
Strip the three letter file extension (such as dQuote(.txt), or
dQuote(.csv)) and the dot from a character vector of filenames}
\usage{
  strip.fileextension(files)
}
\arguments{
  \item{files}{A character vector of file names.}
}
\value{
  A character vector with the file extensions, including
  the dot removed.
}
\description{
  The code looks to be more generic than just three
  characters, but I haven't checked this in much detail
  yet.
}
\examples{
a <- c("file1.txt", "file1.updated.txt")
strip.fileextension(a)
}
\author{
  Mark Cowley, 10 Nov 2006
}
\keyword{manip}

