\name{zip}
\alias{zip}
\title{Create a zip archive.}
\usage{
  zip(x, zip.file, recurse = TRUE, store = FALSE,
    quiet = TRUE)
}
\arguments{
  \item{x}{a vector of file or dir names.}

  \item{zip.file}{the name (including the path) of the zip
  file to be created.  Should end in .zip or .ZIP}

  \item{recurse}{logical: recurse into directories? (-r)}

  \item{store}{logical: store only? (-0), i.e., do not
  compress.}

  \item{quiet}{logical: quiet operation (-q)}
}
\value{
  none. a zip file archive is made.
}
\description{
  This function creates zip archives, either of single
  files, multiple files, a single directory, multiple
  directories, or combinations of files and directories.
  Currently, \code{recurse=TRUE}, which you'll only want to
  disable if you really don't want directories to be
  recursed into (ie it's ok to leave \code{recurse=TRUE} if
  you only have a bunch of files to compress.)
}
\section{Warning}{
  If you specify a vector of filenames, if they are not all
  in the same directory, then the resulting archive
  preserves those different directory paths. i.e., when you
  unzip the archive, you will get a minimal representation
  of the filesystem that captures the different paths of
  the files. You should copy files into the same directory,
  then zip them if you do not want this behaviour! NB this
  copies the default behaviour of using zip on the command
  line. Tested on OSX using Zip 2.32 from the Info-ZIP
  team, which exists at /usr/bin/zip both on OSX (10.5),
  and solaris 10.
}

\section{Todo}{
  just like \code{options("unzip")}, implement an
  \code{options("zip")}
}
\author{
  Mark Cowley, 2009-10-15
}

