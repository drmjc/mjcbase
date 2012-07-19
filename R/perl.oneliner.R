#' execute a perl one liner
#' 
#' execute a perl one liner, such as a regex search & replace (which is how
#' i most often use this), eg \code{s|^.+\\t||} and execute it on the
#' cmdline.
#' 
#' you can pass in multiple commands (such as multiple regex search/replace strings) applied
#' to one or multiple files.
#' i've tested the usage of multiple regex commands applied to a single file.
#' 
#' @details commands
#' The commands vector can either contain the leading perl command, eg
#' \code{perl.oneliner("perl -pi -e 's|<DTG|\n  <DTG|g'", file)}\cr
#' or just a pattern, eg\cr
#' \code{perl.oneliner("'s|<DTG|\n  <DTG|g'", file)}\cr
#' in which case we will add the leading \code{perl -pi -e }
#' 
#' @param cmds a character vector of commands
#' @param files a character vector of paths to files. It is recyled to match the length
#'   of cmds
#' @param intern logical: capture the output of the command as an R character vector?
#' @param debug logical: if \code{TRUE}, print the oneliner command before executing it
#' 
#' @return nothing
#' 
#' @author Mark Cowley
#' @export
perl.oneliner <- function(cmds, files, intern=FALSE, debug=FALSE) {
	files <- recycle(files, length(cmds))
	for(i in 1:length(cmds)) {
		cmd <- cmds[i]
		file <- files[i]

		if( !grepl("^perl", cmd) )
			cmd <- paste("perl -pi -e", cmd)
		
		cmd <- paste(cmd, shQuote(file))
		
		if( debug ) cat(cmd, '\n')
		system(cmd, intern=intern)
	}
}
