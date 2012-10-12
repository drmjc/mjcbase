#' get the dependencies of a package
#' 
#' to install a package, you must have all the packages listed in
#' the Depends and Imports directives in the DESCRIPTION file.
#' You may be quite surprised just how many dependencies there are
#' for some package once you recursively search for them!
#'
#' @param pkg character(1): the package name. this should already be installed.
#' @param recursive logical: search for dependencies recursively?
#' 
#' @return a character vector of package dependencies, in the correct order
#' such that if you had to install them de novo, you could do so from \code{1:n}.
#' If there are none, then a \code{character(0)} is returned.
#' 
#' @author Mark Cowley, 2012-07-30
#' @export
#' @examples
#' \dontrun{
#' package_dependencies("gcrma")
#' # [1] "BiocGenerics"   "Biobase"        "zlibbioc"       "affyio"        
#' # [5] "BiocInstaller"  "preprocessCore" "affy"           "stats4"        
#' # [9] "IRanges"        "Biostrings"     "splines"       
#' 
#' package_dependencies("base")
#' # character(0)
#' 
#' package_dependencies("microarrays")
#' #  [1] "BiocGenerics"   "Biobase"        "gtools"         "gdata"         
#' #  [5] "excelIO"        "mjcbase"        "bitops"         "caTools"       
#' #  [9] "grid"           "KernSmooth"     "MASS"           "gplots"        
#' # [13] "limma"          "mjcgraphics"    "DBI"            "RSQLite"       
#' # [17] "stats4"         "IRanges"        "AnnotationDbi"  "Cairo"         
#' # [21] "genomics"       "RColorBrewer"   "plyr"           "stringr"       
#' # [25] "dichromat"      "colorspace"     "munsell"        "labeling"      
#' # [29] "scales"         "lattice"        "reshape2"       "digest"        
#' # [33] "memoise"        "proto"          "ggplot2"        "xtable"        
#' # [37] "XML"            "annotate"       "splines"        "survival"      
#' # [41] "genefilter"     "methylumi"      "nleqslv"        "zlibbioc"      
#' # [45] "affyio"         "BiocInstaller"  "preprocessCore" "affy"          
#' # [49] "bigmemory"      "GenomicRanges"  "DNAcopy"        "Biostrings"    
#' # [53] "BSgenome"       "genoset"        "RCurl"          "Rsamtools"     
#' # [57] "rtracklayer"    "nlme"           "Matrix"         "mgcv"          
#' # [61] "akima"          "locfit"         "ash"            "mvtnorm"       
#' # [65] "rgl"            "misc3d"         "ks"             "hdrcde"        
#' # [69] "lumi"           "gcrma"          "affyPLM"        "tools"         
#' # [73] "bit"            "ff"             "codetools"      "iterators"     
#' # [77] "foreach"        "oligoClasses"   "affxparser"     "oligo"         
#' # [81] "simpleaffy"     "mjcaffy"        "mjcstats"      
#' 
#' package_dependencies("metaGSEA")
#' #  [1] "gtools"         "gdata"          "excelIO"        "mjcbase"       
#' #  [5] "bitops"         "caTools"        "grid"           "KernSmooth"    
#' #  [9] "MASS"           "gplots"         "limma"          "mjcgraphics"   
#' # [13] "BiocGenerics"   "Biobase"        "DBI"            "RSQLite"       
#' # [17] "stats4"         "IRanges"        "AnnotationDbi"  "xtable"        
#' # [21] "XML"            "annotate"       "tools"          "graph"         
#' # [25] "GSEABase"       "Cairo"          "genomics"       "RColorBrewer"  
#' # [29] "plyr"           "stringr"        "dichromat"      "colorspace"    
#' # [33] "munsell"        "labeling"       "scales"         "lattice"       
#' # [37] "reshape2"       "digest"         "memoise"        "proto"         
#' # [41] "ggplot2"        "splines"        "survival"       "genefilter"    
#' # [45] "methylumi"      "nleqslv"        "zlibbioc"       "affyio"        
#' # [49] "BiocInstaller"  "preprocessCore" "affy"           "bigmemory"     
#' # [53] "GenomicRanges"  "DNAcopy"        "Biostrings"     "BSgenome"      
#' # [57] "genoset"        "RCurl"          "Rsamtools"      "rtracklayer"   
#' # [61] "nlme"           "Matrix"         "mgcv"           "akima"         
#' # [65] "locfit"         "ash"            "mvtnorm"        "rgl"           
#' # [69] "misc3d"         "ks"             "hdrcde"         "lumi"          
#' # [73] "gcrma"          "affyPLM"        "bit"            "ff"            
#' # [77] "codetools"      "iterators"      "foreach"        "oligoClasses"  
#' # [81] "affxparser"     "oligo"          "simpleaffy"     "mjcaffy"       
#' # [85] "mjcstats"       "microarrays"   
#' }
package_dependencies <- function(pkg, recursive = TRUE) {
	deps <- .package_dependencies(pkg, recursive = recursive, debug = FALSE)
	deps <- deps[-length(deps)]
	
	deps
}

# private recursive function. see package_dependencies
.package_dependencies <- function(pkg, recursive = TRUE, debug = FALSE) {
	if( debug ) cat(pkg, ": ")
	deps <- packageDescriptionDependencies(pkg)
	if( debug ) {
		cat(paste(deps, collapse=", "))
		cat("\n")
	}
	if( recursive && length(deps) > 0 ) {
		deps <- as.list(deps)
		for(i in 1:length(deps)) {
			deps[[i]] <- .package_dependencies(deps[[i]], recursive = TRUE)
		}
		deps <- unlist(deps)
	}
	
	deps <- setdiff(c(deps, pkg), "")
	
	deps
}

#' obtain the package dependencies for 1 package
#' 
#' utils provides \code{\link[utils]{packageDescription}}, which parses and returns the
#' \code{DESCRIPTION} file of an installed package. This function
#' retrieves a \code{character vector} of package dependencies
#' required at install time of package \code{pkg}.
#'
#' @param pkg character(1): the package name. this should already be installed.
#' @param suggests logical: report the packages in the Suggests field?
#' @param enhances logical: report the packages in the Enhances field?
#' @param default.packages character vector of default package names.
#' 
#' @return a character vector of length N, for each of the N dependencies. if none
#'  are found, then \code{N=0}.
#' 
#' @author Mark Cowley, 2012-07-30
#' @export
#' @importFrom utils packageDescription
#' 
#' @examples
#' \dontrun{
#' packageDescriptionDependencies("gcrma")
#' # [1] "affy"          "BiocInstaller" "Biobase"       "affyio"       
#' # [5] "IRanges"       "Biostrings"    "splines"
#' packageDescriptionDependencies("gcrma", suggests=T)
#' # [1] "affy"          "BiocInstaller" "Biobase"       "affyio"       
#' # [5] "IRanges"       "Biostrings"    "splines"       "affydata"     
#' # [9] "tools"         "hgu95av2cdf"   "hgu95av2probe"
#' }
packageDescriptionDependencies <- function(pkg, suggests=FALSE, enhances=FALSE, default.packages=c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")) {
	
	pd <- packageDescription(pkg)
	if("Depends" %in% names(pd) && "Imports" %in% names(pd))
		deps <- paste(c(pd$Depends, pd$Imports), collapse=",")
	else if("Depends" %in% names(pd))
		deps <- pd$Depends
	else if("Imports" %in% names(pd))
		deps <- pd$Imports
	else
		deps <- ""
	if( suggests && "Suggests" %in% names(pd) ) deps <- paste(c(deps, pd$Suggests), collapse=",")
	if( enhances && "Enhances" %in% names(pd) ) deps <- paste(c(deps, pd$Enhances), collapse=",")
	
	deps <- .parse.dependencies(deps, exclude.R = TRUE)
	deps <- setdiff(deps, default.packages)
	if( identical(deps, "") ) deps <- character(0)
	deps
}

# MJC from roxygen2 (which needs R >= 2.14.1)
.parse.dependencies <- function(pkgs, exclude.R = TRUE) {
  if (!identical(pkgs, NULL) && !identical(pkgs, "")) {
    pkgs <- strsplit(pkgs, ",")[[1]]
    pkgs <- gsub("^\\s+|\\s+$", "", pkgs)
    pkg.ver <- pkgs
    pkgs <- gsub("\\s*\\(.*?\\)", "", pkgs)
    names(pkgs) <- pkg.ver
    if (exclude.R) {
      pkgs <- pkgs[pkgs != "R"]
    }
  }
  return( pkgs )
}
