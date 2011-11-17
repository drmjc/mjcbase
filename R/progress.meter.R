## Initialise a progress meter to peform N iterations, and plot the specified
## symbol at each appropriate interval to fill and entire line of an Rcmd session
##
## Mark Cowley, 18 April 2006
##
init.progress.meter <- function(N, symbol=".") { progress.meter(0, N, symbol) }


## Update the progress meter that MUST have been setup using
##	   init.progress.meter(N)
## If appropriate, a tick is printed.
##
## Mark Cowley, 18 April 2006
##
update.progress.meter <- function() { progress.meter() }


## Primitive GUI for displaying a hashmark until there are options()$width
## hashmarks.
##
## NB: see ?init.progress.meter and ?update.progress.meter for simpler
##	   interfaces to displaying a progress meter.
##
## Parameters:
##	   iter: the iteration that you are upto
##	   N: that max no of iterations possible
##	   symbol: the symbol to display (default is ".")
##
## Notes:
##	   you can initialise the progress meter with:
##		   progress.meter(0, N, ".")
##	   then iteratively call just 'progress.meter()'
##		which remembers and updates the iteration
##
## Mark Cowley, 18 April 2006
##
progress.meter <- function(iter, N, symbol) {
	DEFAULT.SYMBOL <- "."

	#
	# This code emulates static variables in R:
	#  the first time the progress.meter is called with
	#  values for iter and N, they are saved to a different
	#  environment, and upon subsequent progress.meter calls,
	#  the values are retrieved and updated from this alternate
	#  environment.
	#
	if( !missing(iter) && iter == 0 ) {
		#
		# Then we are setting up the progress meter for
		# the first time.
		#
		# assign("iter", iter, pos=grep("mjcbase", searchpaths()))
		# assign("N", N, pos=grep("mjcbase", searchpaths()))
		options(pm.iter=iter)
		options(pm.N=N)
		if( missing(symbol) ) {
			symbol <- DEFAULT.SYMBOL
		}
		# assign("symbol", symbol, pos=grep("mjcbase", searchpaths()))
		options(pm.symbol=symbol)

	}
	else if(missing(iter)) {
		# iter <- get("iter", pos=grep("mjcbase", searchpaths())) + 1
		iter <- getOption(pm.iter)
	}
	#
	# else
	#	  iter has been supplied and is non-zero for use where
	#	  the progress.meter(i, N) is called in a loop.
	#
	if( missing(N) ) {
		# N <- get("N", pos=grep("mjcbase", searchpaths()))
		N <- getOption(pm.N)
	}

	if( missing(symbol) ) {
		# symbol <- get("symbol", pos=grep("mjcbase", searchpaths()))
		symbol <- getOption(pm.symbol)
	}

	#
	# how much room is there for plotting characters?
	#
	width <- floor( options()$width / nchar(symbol) )

	#
	# How often should a new tick be printed?
	#
	binsize <- (N / width)

	if( iter != 0 && iter %% N == 0 ) {
		#
		# if the user forgets to reset progress.meter() then the iter will start at N instead of 0,
		#  thus use the %% instead of if 'iter == N'
		#
		cat(p(symbol, "\n"))
	}
	else if( iter != 0 && floor(iter/binsize) > floor((iter-1)/binsize) ) {
		# ie the multiple of binsize has just increased by one with this
		# iteration.
		cat(symbol)
	}

	# assign("iter", iter, pos=grep("mjcbase", searchpaths()))
	options(pm.iter=iter)
	
}


## as above but for a percentage style progress meter
## [   0% ]
## [   1% ]
## ...
## [  99% ]
## [ 100% ]
##
## Mark Cowley, 19 April 2006
##
init.progress.meter2 <- function( N ) { progress.meter2(0, N) }


## as above but for a percentage style progress meter
## [   0% ]
## [   1% ]
## ...
## [  99% ]
## [ 100% ]
##
## Mark Cowley, 19 April 2006
##
update.progress.meter2 <- function() { progress.meter2() }


## as above but for a percentage style progress meter
## [   0% ]
## [   1% ]
## ...
## [  99% ]
## [ 100% ]
##
## Mark Cowley, 19 April 2006
##
progress.meter2 <- function(iter, N) {

	#
	# This code emulates static variables in R:
	#  the first time the progress.meter is called with
	#  values for iter and N, they are saved to a different
	#  environment, and upon subsequent progress.meter calls,
	#  the values are retrieved and updated from this alternate
	#  environment.
	#
	if( !missing(iter) && iter == 0 ) {
		#
		# Then we are setting up the progress meter for
		# the first time.
		#
		# assign("iter", iter, pos=grep("mjcbase", searchpaths()))
		# assign("N", N, pos=grep("mjcbase", searchpaths()))
		options(pm.iter=iter)
		options(pm.N=N)

	}
	else if(missing(iter)) {
		# iter <- get("iter", pos=grep("mjcbase", searchpaths())) + 1
		iter <- getOption("pm.iter")
	}
	#
	# else
	#	  iter has been supplied and is non-zero for use where
	#	  the progress.meter(i, N) is called in a loop.
	#
	if( missing(N) ) {
		# N <- get("N", pos=grep("mjcbase", searchpaths()))
		N <- getOption("pm.N")
	}

	#
	# how much room is there for plotting characters?
	#
	width <- nchar( "[ 100% ]" )

	#
	# How often should a new tick be printed?
	#
	binsize <- N / 100 # every percent

	if( iter != 0 && iter %% N == 0 ) {
		#
		# if the user forgets to reset progress.meter() then the iter will start at N instead of 0,
		#  thus use the %% instead of if 'iter == N'
		#
		cat( strcat(rep("\08", width)))
		cat( "[ 100% ]\n" )
	}
	else if( iter != 0 && floor(iter/binsize) > floor((iter-1)/binsize) ) {
		# ie the multiple of binsize has just increased by one with this
		# iteration.
		percent <- floor(iter/binsize)
		cat( strcat(rep("\08", width)))
		cat( p("[ ", sprintf( "%3s", percent ), "% ]") )
	}
	else if( iter == 1 ) {
		percent <- 0
		cat( "[	  0% ]" )
	}
	# assign("iter", iter, pos=grep("mjcbase", searchpaths()))
	options(pm.iter=iter)
	
}
