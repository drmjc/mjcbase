\name{pbpaste}
\alias{paste.from.clipboard}
\alias{pbpaste}
\title{Paste the OSX clipboard into a character vector}
\usage{
  pbpaste(class = "character", sep = "\t", ok = TRUE,
    warn = TRUE)

  paste.from.clipboard(class = "character", sep = "\t")
}
\arguments{
  \item{class}{the class of the object to be created}

  \item{sep}{the separator to use if \code{class="matrix"},
  or \code{class="data.frame"}}

  \item{ok}{logical: is it OK to be missing a final EOL?}

  \item{warn}{logical: warn if the clipboard is missing a
  final EOL}
}
\value{
  a character vector of lines
}
\description{
  Enhanced control over object type\cr The default is to
  read from clipboad into a character vector, and this will
  always work. You can attempt to convert this to other
  objects, like \code{matrix} or \code{data.frame}, if the
  clipboard contains suitable data (eg multi-line strings
  with tab's or comma's separating fields). We have tested
  this using vectors, 1 element per line; list, where each
  line becomes its own list element, matrix and data.frame,
  where each line is a row, each column is tab separated;
  also a Date class (see examples).
}
\details{
  If you need finer control when importing to \code{matrix}
  or \code{data.frame}, we recommend you use
  \code{read.table(..., file=pipe("pbpaste"), ...)}.

  You can copy & paste actual representations of an R
  object complete with attributes & names etc using
  \code{dput}/\code{dget}. see examples.
}
\examples{
\dontrun{
  pbcopy(letters)
  input <- pbpaste()
  input <- pbpaste("character")

  pbcopy(1:10)
  pbpaste()
  pbpaste("numeric")

  df <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5)
  pbcopy(df)
  input <- pbpaste("data.frame")
  input <- pbpaste("matrix")
  input <- pbpaste("list")
  input <- pbpaste("Date") # this should fail

 pbcopy(Sys.Date())
 # structure(15286, class = "Date")
 pbpaste() # the class parameter is ignored, since the clipboard already knows its class
 pbpaste("character") # the class parameter is ignored, since the clipboard already knows its class

# here's a way to copy the actual object represntation to & from the clipboad
# (great for copying objects between R sessions)
dput(letters, pipe("pbpaste", "w"))
dget(pipe("pbcopy"))

}
}
\author{
  Mark Cowley, 2011-07-22
}
\seealso{
  \code{\link{readLines}}, \code{\link{pbcopy}}, the
  \dQuote{Clipboard} section within ?\code{\link{file}}
}

