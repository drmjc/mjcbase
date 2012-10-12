\name{package_dependencies}
\alias{package_dependencies}
\title{get the dependencies of a package}
\usage{
  package_dependencies(pkg, recursive = TRUE)
}
\arguments{
  \item{pkg}{character(1): the package name. this should
  already be installed.}

  \item{recursive}{logical: search for dependencies
  recursively?}
}
\value{
  a character vector of package dependencies, in the
  correct order such that if you had to install them de
  novo, you could do so from \code{1:n}. If there are none,
  then a \code{character(0)} is returned.
}
\description{
  to install a package, you must have all the packages
  listed in the Depends and Imports directives in the
  DESCRIPTION file. You may be quite surprised just how
  many dependencies there are for some package once you
  recursively search for them!
}
\examples{
\dontrun{
package_dependencies("gcrma")
# [1] "BiocGenerics"   "Biobase"        "zlibbioc"       "affyio"
# [5] "BiocInstaller"  "preprocessCore" "affy"           "stats4"
# [9] "IRanges"        "Biostrings"     "splines"

package_dependencies("base")
# character(0)

package_dependencies("microarrays")
#  [1] "BiocGenerics"   "Biobase"        "gtools"         "gdata"
#  [5] "excelIO"        "mjcbase"        "bitops"         "caTools"
#  [9] "grid"           "KernSmooth"     "MASS"           "gplots"
# [13] "limma"          "mjcgraphics"    "DBI"            "RSQLite"
# [17] "stats4"         "IRanges"        "AnnotationDbi"  "Cairo"
# [21] "genomics"       "RColorBrewer"   "plyr"           "stringr"
# [25] "dichromat"      "colorspace"     "munsell"        "labeling"
# [29] "scales"         "lattice"        "reshape2"       "digest"
# [33] "memoise"        "proto"          "ggplot2"        "xtable"
# [37] "XML"            "annotate"       "splines"        "survival"
# [41] "genefilter"     "methylumi"      "nleqslv"        "zlibbioc"
# [45] "affyio"         "BiocInstaller"  "preprocessCore" "affy"
# [49] "bigmemory"      "GenomicRanges"  "DNAcopy"        "Biostrings"
# [53] "BSgenome"       "genoset"        "RCurl"          "Rsamtools"
# [57] "rtracklayer"    "nlme"           "Matrix"         "mgcv"
# [61] "akima"          "locfit"         "ash"            "mvtnorm"
# [65] "rgl"            "misc3d"         "ks"             "hdrcde"
# [69] "lumi"           "gcrma"          "affyPLM"        "tools"
# [73] "bit"            "ff"             "codetools"      "iterators"
# [77] "foreach"        "oligoClasses"   "affxparser"     "oligo"
# [81] "simpleaffy"     "mjcaffy"        "mjcstats"

package_dependencies("metaGSEA")
#  [1] "gtools"         "gdata"          "excelIO"        "mjcbase"
#  [5] "bitops"         "caTools"        "grid"           "KernSmooth"
#  [9] "MASS"           "gplots"         "limma"          "mjcgraphics"
# [13] "BiocGenerics"   "Biobase"        "DBI"            "RSQLite"
# [17] "stats4"         "IRanges"        "AnnotationDbi"  "xtable"
# [21] "XML"            "annotate"       "tools"          "graph"
# [25] "GSEABase"       "Cairo"          "genomics"       "RColorBrewer"
# [29] "plyr"           "stringr"        "dichromat"      "colorspace"
# [33] "munsell"        "labeling"       "scales"         "lattice"
# [37] "reshape2"       "digest"         "memoise"        "proto"
# [41] "ggplot2"        "splines"        "survival"       "genefilter"
# [45] "methylumi"      "nleqslv"        "zlibbioc"       "affyio"
# [49] "BiocInstaller"  "preprocessCore" "affy"           "bigmemory"
# [53] "GenomicRanges"  "DNAcopy"        "Biostrings"     "BSgenome"
# [57] "genoset"        "RCurl"          "Rsamtools"      "rtracklayer"
# [61] "nlme"           "Matrix"         "mgcv"           "akima"
# [65] "locfit"         "ash"            "mvtnorm"        "rgl"
# [69] "misc3d"         "ks"             "hdrcde"         "lumi"
# [73] "gcrma"          "affyPLM"        "bit"            "ff"
# [77] "codetools"      "iterators"      "foreach"        "oligoClasses"
# [81] "affxparser"     "oligo"          "simpleaffy"     "mjcaffy"
# [85] "mjcstats"       "microarrays"
}
}
\author{
  Mark Cowley, 2012-07-30
}

