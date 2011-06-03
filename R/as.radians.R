#'
#' Convert values from degrees to radians.
#'
#' @param x a vector
#' @param strict fail if attr(x, "units") is not "degrees"
#' @return a vector
#' @export
#'
as.radians <- function(x, strict=FALSE) {
	x.units <- attr(x, "units")
	if(is.null(x.units)) {
		if(strict) {
			stop("No units supplied, and strict is TRUE")
		} else {
			x <- pi / 180.0 * x
			attr(x, "units") <- "radians"
		}
	} else if(x.units == "radians") {
		# leave x unchanged
	} else if(x.units == "degrees") { 
		x <- pi / 180.0 * x
		attr(x, "units") <- "radians"
	} else {
		stop("I don't know what to do with a value in units of", x.units)
	}
	return(x)
}