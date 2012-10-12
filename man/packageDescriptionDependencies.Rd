\name{packageDescriptionDependencies}
\alias{packageDescriptionDependencies}
\title{obtain the package dependencies for 1 package}
\usage{
  packageDescriptionDependencies(pkg, suggests = FALSE,
    enhances = FALSE,
    default.packages = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"))
}
\arguments{
  \item{pkg}{character(1): the package name. this should
  already be installed.}

  \item{suggests}{logical: report the packages in the
  Suggests field?}

  \item{enhances}{logical: report the packages in the
  Enhances field?}

  \item{default.packages}{character vector of default
  package names.}
}
\value{
  a character vector of length N, for each of the N
  dependencies. if none are found, then \code{N=0}.
}
\description{
  utils provides \code{\link[utils]{packageDescription}},
  which parses and returns the \code{DESCRIPTION} file of
  an installed package. This function retrieves a
  \code{character vector} of package dependencies required
  at install time of package \code{pkg}.
}
\examples{
\dontrun{
packageDescriptionDependencies("gcrma")
# [1] "affy"          "BiocInstaller" "Biobase"       "affyio"
# [5] "IRanges"       "Biostrings"    "splines"
packageDescriptionDependencies("gcrma", suggests=T)
# [1] "affy"          "BiocInstaller" "Biobase"       "affyio"
# [5] "IRanges"       "Biostrings"    "splines"       "affydata"
# [9] "tools"         "hgu95av2cdf"   "hgu95av2probe"
}
}
\author{
  Mark Cowley, 2012-07-30
}

