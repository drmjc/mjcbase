dir.Rdata <- function(path=".") {
  dir(path=path, pattern=".R[dD]ata$|.R[dD]ata.gz$|Rda|RDa|Rda.gz|RDa.gz")
}
