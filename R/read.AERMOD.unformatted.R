#'
#' Read an "unformatted" (binary) AERMOD output file.
#'
#' @param filename file to read
#' @param n.receptors number of receptors in the model
#' @return a list of records, one per modelled interval. Each record contains a date (to the hour); 
#'   the length of the averaging period; the group ID; and a list of concentrations, one per receptor.
#' @export
#' @references
#' EPA (2004). User's Guide for the AMS/EPA Regulatory Model AERMOD. EPA-454/B-03-001.
#'
read.AERMOD.unformatted <- function(filename, n.receptors, endian=.Platform$endian) {
	require(lubridate)
	con <- file(filename, open='rb')
	read.record <- function(con) {
		YYMMDDHH <- readBin(con, 'integer', size=4, signed=FALSE, endian=endian)
		avg.period <- readBin(con, 'integer', size=4, signed=FALSE, endian=endian)
		group.id <- readChar(con, 8)
		concentrations <- readBin(con, 'double', n=n.receptors, endian=endian)
		return(list(
			date = as.Date(ymd(floor(YYMMDDHH / 100))),
			hour = YYMMDDHH %% 100,
			avg.period = avg.period,
			group.id = sub("[ ]+$", "", group.id),
			concentrations = concentrations))
	}	
	records <- list()
	while(magic <- length(readBin(con, 'integer', size=4))) {
		rec <- read.record(con)
		records <- c(records, list(rec))
		magic <- readBin(con, 'integer', size=4)
	}
	close(con)
	return(records)
}
filename <- '/Users/holstius/Dropbox/Projects/aermod-11103/src/aermod_source/24ALL.BIN'
stopifnot(file.exists(filename))
n.receptors <- 144
read.AERMOD.unformatted(filename, n.receptors)
