#'
#' Generate a polar grid.
#'
#' @param r distances (arbitrary units)
#' @param n angles
#' @param offset starting angle
#' @param increment angualr increment
#' @param units (optional) angular units ("degrees" or "radians")
#' @param origin (optional) x, y offset
#' @return a matrix
#' @export
#' @references
#' EPA (2004). User's Guide for the AMS/EPA Regulatory Model AERMOD. EPA-454/B-03-001.
#'
grid.polar <- function(r, n, offset, increment, units="degrees", origin=c(0,0)) {
	theta <- offset + increment * rep(1:n, each=length(r))
	attr(theta, "units") <- units
	theta <- as.radians(theta, strict=TRUE)
	x <- r * cos(theta)
	y <- r * sin(theta)
	return(cbind(x,y,r,theta))
}