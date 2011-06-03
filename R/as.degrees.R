#'
#' Convert values from radians to degrees.
#'
#' @param x a vector
#' @param strict fail if attr(x, "units") is not "radians"
#' @return a vector
#' @export
#'
as.degrees <- function(x, strict=FALSE) {
	x.units <- attr(x, "units")
	if(is.null(x.units)) {
		if(strict) {
			stop("No units supplied, and strict is TRUE")
		} else {
			x <- 180.0 / pi * x
			attr(x, "units") <- "degrees"
		}
	} else if(x.units == "degrees") {
		# leave x unchanged
	} else if(x.units == "radians") { 
		x <- 180.0 / pi * x
		attr(x, "units") <- "degrees"
	} else {
		stop("I don't know what to do with a value in units of", x.units)
	}
	return(x)
}