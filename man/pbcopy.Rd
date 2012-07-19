\name{pbcopy}
\alias{copy.to.clipboard}
\alias{pbcopy}
\title{Write a vector to the OSX clipboard}
\usage{
  pbcopy(x)

  copy.to.clipboard(x)
}
\arguments{
  \item{x}{an object to be writen to the clipboard. see
  Details.}
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

