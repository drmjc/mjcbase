\docType{methods}
\name{read.table.fast}
\alias{read.csv.fast}
\alias{read.delim.fast}
\alias{read.table.fast}
\title{read.delim fast, on a subset of columns}
\usage{
  read.table.fast(file, skip = 0, nrows = -1,
    header = TRUE, row.names, sep = "", ...,
    columns = NULL)

  read.delim.fast(file, header = TRUE, sep = "\t",
    quote = "\"", dec = ".", fill = TRUE,
    comment.char = "", ..., columns = NULL)

  read.csv.fast(file, header = TRUE, sep = ",",
    quote = "\"", dec = ".", comment.char = "", ...,
    columns = NULL)
}
\arguments{
  \item{\dots}{arguments passed to
  \code{\link{read.table}}}

  \item{columns}{a character vector of column names to
  import; or a numeric vector of column indices to import;
  or a logical vector same length as ncol(file)}

  \item{file}{the name of the file which the data are to be
  read from.  Each row of the table appears as one line of
  the file.  If it does not contain an \emph{absolute}
  path, the file name is \emph{relative} to the current
  working directory, \code{\link{getwd}()}. Tilde-expansion
  is performed where supported.  As from \R 2.10.0 this can
  be a compressed file (see \code{\link{file}}).

  Alternatively, \code{file} can be a readable text-mode
  \link{connection} (which will be opened for reading if
  necessary, and if so \code{\link{close}}d (and hence
  destroyed) at the end of the function call).  (If
  \code{\link{stdin}()} is used, the prompts for lines may
  be somewhat confusing.  Terminate input with a blank line
  or an EOF signal, \code{Ctrl-D} on Unix and \code{Ctrl-Z}
  on Windows.  Any pushback on \code{stdin()} will be
  cleared before return.)

  \code{file} can also be a complete URL.  (For the
  supported URL schemes, see the \sQuote{URLs} section of
  the help for \code{\link{url}}.)  }

  \item{skip}{integer: the number of lines of the data file
  to skip before beginning to read data.}

  \item{nrows}{integer: the maximum number of rows to read
  in.  Negative and other invalid values are ignored.}

  \item{header}{a logical value indicating whether the file
  contains the names of the variables as its first line.
  If missing, the value is determined from the file format:
  \code{header} is set to \code{TRUE} if and only if the
  first row contains one fewer field than the number of
  columns.}

  \item{row.names}{a vector of row names.  This can be a
  vector giving the actual row names, or a single number
  giving the column of the table which contains the row
  names, or character string giving the name of the table
  column containing the row names.

  If there is a header and the first row contains one fewer
  field than the number of columns, the first column in the
  input is used for the row names.  Otherwise if
  \code{row.names} is missing, the rows are numbered.

  Using \code{row.names = NULL} forces row numbering.
  Missing or \code{NULL} \code{row.names} generate row
  names that are considered to be \sQuote{automatic} (and
  not preserved by \code{\link{as.matrix}}).  }

  \item{sep}{the field separator character.  Values on each
  line of the file are separated by this character.  If
  \code{sep = ""} (the default for \code{read.table}) the
  separator is \sQuote{white space}, that is one or more
  spaces, tabs, newlines or carriage returns.}

  \item{quote}{the set of quoting characters. To disable
  quoting altogether, use \code{quote = ""}.  See
  \code{\link{scan}} for the behaviour on quotes embedded
  in quotes.  Quoting is only considered for columns read
  as character, which is all of them unless
  \code{colClasses} is specified.}

  \item{dec}{the character used in the file for decimal
  points.}

  \item{fill}{logical. If \code{TRUE} then in case the rows
  have unequal length, blank fields are implicitly added.
  See \sQuote{Details}.}

  \item{comment.char}{character: a character vector of
  length one containing a single character or an empty
  string.  Use \code{""} to turn off the interpretation of
  comments altogether.}
}
\value{
  a \code{data.frame} containing just the columns of
  interest, just like you'd get from
  \code{\link{read.table}}, only much faster.
}
\description{
  If you have a huge file & are only interested in some of
  the columns, \code{read.delim.fast} can be much much
  faster. It depends on how many columns there are. If you
  have a VERY fast file, then \code{\link{read.table.fast}}
  and subsetting columns after may still be faster.
}
\details{
  Some testing on a 1134514 x 32 column file (an Illumina
  Omni1M TXT file), extracting 5 columns of interest saw a
  4.8x speedup (8.5 vs 40.5 sec).
}
\note{
  WARNING In general this code works really well when there
  are the same number of column names as there are columns.
  It works great on tsv and csv files not produced by \R,
  since they almost always have the same number of colnames
  as columns.\cr \R likes to do some strange things with
  \code{\link{write.table}} and \code{\link{write.csv}},
  where if row.names=TRUE, then there will be 1 fewer
  colnames as there are columns of data. When using
  \code{\link{read.table}} and \code{\link{read.csv}}, this
  signals that the 1st column should be the
  \code{row.names}, then the subsequent columns are cols
  \code{1-N}. Since this code uses GNU \code{cut}, it's not
  aware of this & will likely shift the colnames and
  columns.
}
\section{timing}{
  a <- read.delim.fast(file, skip=11, nrows=10,
  columns=1:5) \cr system.time(a <- read.delim.fast(file,
  skip=11, nrows=1134514, columns=1:5)) \cr # user system
  elapsed \cr # 7.550 2.240 8.457 \cr system.time(a <-
  read.delim(file, skip=11, nrows=1134514)[,1:5]) \cr #
  user system elapsed \cr # 39.070 1.190 40.451 \cr
}
\examples{
##
## read.table.fast examples\\cr
##
test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
tf <- tempfile()

write.table(test1, tf)
# [1] "\\"a\\" \\"b\\" \\"c\\" \\"d\\""
# [2] "\\"1\\" \\"a\\" \\"A\\" 1 0.379498970282498"
# note the missing leading blank -- cut is NOT happy with this
readLines(tf)
read.table(tf)[,1:4]
# read.table.fast(tf, columns=1:4)

unlink(tf)
##
## read.delim.fast examples\\cr
##
\dontrun{
file <- "~/tmp/ASCAT/TXT/ICGC_ABMP_20100506_11_ND_5486142165_R03C01.txt"
system.time(
  a <- read.delim.fast(file, skip=11, nrows=1134514, columns=1:5)
)

system.time(
    a <- read.delim.fast(file, skip=10, nrows=1134514, check.names=FALSE, columns=c("SNP Name", "Sample ID", "Chr", "Position", "B Allele Freq", "Log R Ratio"))
)
}

test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
tf <- tempfile()
# pwbc::write.delim default keeps ncol or data and colnames in sync
write.delim(test1, tf)
readLines(tf)
# [1] "a\\tb\\tc\\td"
# [2] "a\\tA\\t1\\t0.379498970282498"
# ...
# note no rownames or leading blank in column. cut is happy
read.delim(tf)[,1:3]
read.delim.fast(tf, columns=1:3)

unlink(tf)
##
## read.csv examples
##
test1 <- data.frame(a=letters[1:5], b=LETTERS[1:5], c=1:5, d=rnorm(5))
tf <- tempfile()

write.csv(test1, tf)
readLines(tf)
# [1] "\\"\\",\\"a\\",\\"b\\",\\"c\\",\\"d\\""
# [2] "\\"1\\",\\"a\\",\\"A\\",1,0.379498970282498"
# ...
# ^^ note the leading blank cell. 'cut' is happy
read.csv(tf)[,1:4]
read.csv.fast(tf, columns=1:4)
# both get this equally wrong by putting the old rownames into the new column1
read.csv(tf)[,1:5]
read.csv.fast(tf, columns=1:5)
# see? there should only be 4 columns
read.csv(tf, row.names=1)[,1:4]
read.csv.fast(tf, columns=1:4, row.names=1)
read.csv.fast(tf, columns=1:4, header=FALSE, nrows=2)
#   V1 V2 V3 V4
# 1 NA  a  b  c
# 2  1  a  A  1
read.csv.fast(tf, columns=1:4, header=TRUE, nrows=2)
#   X a b c
# 1 1 a A 1
# 2 2 b B 2
read.csv.fast(tf, columns=1:4, header=TRUE, nrows=2, row.names=1)
#   a b c      d
# 1 a A 1 -1.569
# 2 b B 2  0.976
read.csv.fast(tf, columns=c(1,2,4), header=TRUE, nrows=2, row.names=1)
#   a b      d
# 1 a A -1.569
# 2 b B  0.976

unlink(tf)
}
\author{
  Mark Cowley, 2012-06-22
}

