get.char <- function(x, idx) {
  unlist(strsplit(x, ""))[idx]
}

to.char.array <- function(x) {
  unlist(strsplit(x, ""))
}
