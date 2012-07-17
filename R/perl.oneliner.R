#' Function to wrap up a perl command such as 's|^.+\\t||' and execute it on the
#' cmdline.
#' 
#' you can pass in multiple commands (such as multiple regex strings) applied
#' to one or multiple files.
#' i've tested the usage of multiple regex commands applied to a single file.
#' 
#' @param cmds a character vector of commands
#' @param files a character vector of paths to files. It is recyled to match the length
#'   of cmds
#' @param intern logical: capture the output of the command as an R character vector?
#' @param debug logical: if \code{TRUE}, print the oneliner command before executing it
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
