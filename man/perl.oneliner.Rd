\name{perl.oneliner}
\alias{perl.oneliner}
\title{Function to wrap up a perl command such as 's|^.+\\t||' and execute it on the
cmdline.}
\usage{
  perl.oneliner(cmds, files, intern = FALSE, debug = FALSE)
}
\arguments{
  \item{cmds}{a character vector of commands}

  \item{files}{a character vector of paths to files. It is
  recyled to match the length of cmds}

  \item{intern}{logical: capture the output of the command
  as an R character vector?}

  \item{debug}{logical: if \code{TRUE}, print the oneliner
  command before executing it}
}
\description{
  you can pass in multiple commands (such as multiple regex
  strings) applied to one or multiple files. i've tested
  the usage of multiple regex commands applied to a single
  file.
}
\author{
  Mark Cowley
}

