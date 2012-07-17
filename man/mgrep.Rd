\name{mgrep}
\alias{grepf}
\alias{grepfl}
\alias{mgrep}
\alias{mgrepl}
\title{multi-grep methods}
\usage{
  mgrep(patterns, x, ignore.case = FALSE, perl = FALSE,
    value = FALSE, fixed = FALSE, useBytes = FALSE,
    invert = FALSE, nomatch = NA)

  mgrepl(patterns, x, ignore.case = FALSE, perl = FALSE,
    fixed = FALSE)

  grepf(patterns, x, ignore.case = FALSE, perl = FALSE,
    value = FALSE, fixed = FALSE, useBytes = FALSE,
    invert = FALSE)

  grepfl(patterns, x, ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE, invert = FALSE)
}
\arguments{
  \item{patterns}{a character vector of at least one
  pattern.}

  \item{x}{a character vector where matches are sought, or
  an object which can be coerced by \code{as.character} to
  a character vector.}

  \item{nomatch}{if a pattern isn't found, what should be
  returned for that element in the result \code{list}?
  default=\code{NA} which just reports an \code{NA}.
  \code{NULL} is allowed, in which case a \code{numeric(0)}
  or \code{character(0)} (if \code{value=TRUE}) is put in
  the corresponding element of the list.}

  \item{ignore.case}{if \code{FALSE}, the pattern matching
  is \emph{case sensitive} and if \code{TRUE}, case is
  ignored during matching.}

  \item{perl}{logical.  Should perl-compatible regexps be
  used?}

  \item{value}{if \code{FALSE}, a vector containing the
  (\code{integer}) indices of the matches determined by
  \code{grep} is returned, and if \code{TRUE}, a vector
  containing the matching elements themselves is returned.}

  \item{fixed}{logical.  If \code{TRUE}, \code{pattern} is
  a string to be matched as is.  Overrides all conflicting
  arguments.}

  \item{useBytes}{logical.  If \code{TRUE} the matching is
  done byte-by-byte rather than character-by-character.
  See \sQuote{Details}.}

  \item{invert}{logical.  If \code{TRUE} return indices or
  values for elements that do \emph{not} match.}
}
\value{
  \code{mgrep}: a \code{list} with one element per pattern
  in the \code{patterns} vector.  Each element of this
  \code{list} is the value returned by \code{\link{grep}}
  for each pattern, OR \code{nomatch} if nothing is found.
  NB this is different from the default behaviour of
  \code{\link{grep}}, which returns \code{numeric(0)} when
  no match is found.

  \code{mgrepl}: a logical vector, \code{length =
  length(patterns)}, indicating whether each pattern was
  found in \code{x}

  \code{grepf}: if \code{value=FALSE}, a \code{numeric}
  vector of indices in \code{1:length(x)} which match
  \emph{any} of the \code{patterns}, or, if
  \code{value=TRUE}, return a \code{character} vector of
  the elements of \code{x} which match \emph{any} of the
  patterns.

  \code{grepfl}: a \code{logical} vector, of
  length\code{=length(x)}, indicating whether each of the
  elements in \code{x} match \emph{any} of the
  \code{patterns}. if \code{invert=TRUE}, then the logical
  vector indicates which elements did not match \emph{any}
  of the \code{patterns}.
}
\description{
  a collection of multi-grep methods which make use of
  \emph{multiple} patterns, as opposed to
  \code{\link[base]{grep}}'s \emph{single} pattern.
}
\details{
  \code{mgrep}: \code{mgrep} takes a vector of
  \code{patterns}, and searches for each within \code{x},
  returning either the matching indices into \code{x}, or
  values (if \code{value=TRUE}), for each pattern in a
  \code{list} of \code{length(patterns)}. Just like
  \code{\link[base]{mget}} as the multi-version of
  \code{\link[base]{get}}, there is a \code{nomatch}
  parameter controlling what to do if no match for a
  particular pattern is found.

  \code{mgrepl}: grep for multiple \code{patterns}, and
  return logical if each pattern is present in any value of
  \code{x}

  \code{grepf}: grep -f for R. ie return the elements of
  \code{x} which match \emph{any} of the \code{patterns}.

  \code{grepfl}: combo of \code{grepf} + \code{grepl} ie
  which elements of \code{x} match \code{>=1} of the
  \code{patterns}, as a logical vector
}
\examples{
# use the state.name example data
mgrep(c("New","^V", "Iran"), state.name)
mgrep(c("New","^V", "Iran"), state.name, value=TRUE)

# nomatch
mgrep(c("New","^V", "Iran"), state.name, nomatch=NULL)
mgrep(c("New","^V", "Iran"), state.name, value=TRUE, nomatch=NULL)
# use the state.name example data
mgrepl(c("New","^V", "Iran"), state.name)
#  New    ^V  Iran
# TRUE  TRUE FALSE
# use the state.name example data
grepf(c("New","^V", "Iran"), state.name)
# [1] 29 30 31 32 45 46
grepf(c("New","^V", "Iran"), state.name, value=TRUE)
grepf(c("New","^V", "Iran"), state.name, invert=TRUE)
# use the state.name example data
grepfl(c("New","^V", "Iran"), state.name)
grepfl(c("New","^V", "Iran"), state.name, invert=TRUE)
}
\author{
  Mark Cowley, 23 Sept 2005

  Mark Cowley, 2011-02-18

  Mark Cowley, 2011-02-18

  Mark Cowley, 2011-02-18
}

