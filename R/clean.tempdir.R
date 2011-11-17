## Function to delete all of the files in the tempdir.
##
## Mark Cowley, 5 April 2006
##
clean.tempdir <- function() {
    files <- dir(path=tempdir(), full=T)
    tmp <- file.remove(files)

    if(length(tmp) == 0 || all(tmp))
        cat("tmpdir is now empty\n")

}
