\name{pbcopy}
\alias{copy.to.clipboard}
\alias{pbcopy}
\title{Write a vector to the OSX clipboard}
\usage{
  pbcopy(x, row.names = FALSE, col.names = TRUE)

  copy.to.clipboard(x)
}
\arguments{
  \item{x}{an object to be writen to the clipboard. see
  Details.}

  \item{row.names}{if \code{x} is a \code{matrix} or
  \code{data.frame}, then either \code{TRUE}, \code{FALSE},
  \code{NULL} \code{NA}, or a \code{character(1)}.  see
  \code{\link{write.delim}}. If \code{x} is a \code{list},
  then write out the list names in each row? see
  \code{\link{names}} arg in \code{\link{writeLines.list}}.
  Otherwise it's ignored.}

  \item{col.names}{if \code{x} is a \code{matrix} or
  \code{data.frame}, then write out the col.names?
  \code{default=TRUE} Otherwise, it's ignored.}
}
\value{
  nothing
}
\description{
  The input, \code{x} will be written to the clipboad,
  using one of these functions: \tabular{ll}{ \code{vector}
  \tab \code{writeLines} \cr \code{list} \tab
  \code{writeLines.list} \cr \code{matrix} \tab
  \code{write.delim} \cr \code{data.frame} \tab
  \code{write.delim} \cr other \tab \code{dput} }
}
\examples{
\dontrun{
pbcopy(letters)
pbcopy(matrix(letters[1:25], 5))
pbcopy(data.frame(letters,LETTERS,1:26))
pbcopy(list(a=letters, b=LETTERS, c=as.character(1:26)))
pbcopy(Sys.Date())
}
}
\author{
  Mark Cowley, 2011-11-08
}
\seealso{
  \code{\link{pbcopy}}, the \dQuote{Clipboard} section
  within ?\code{\link{file}}
}

