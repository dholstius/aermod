#'
#' Generate a polar grid.
#'
#' @param r distances (arbitrary units)
#' @param n angles
#' @param offset starting angle
#' @param increment angular increment
#' @param default.units if offset or increment do not have units, assume these
#' @param origin (optional) x, y offset
#' @return a SpatialPointsDataFrame
#' @export
#' @references
#' EPA (2004). User's Guide for the AMS/EPA Regulatory Model AERMOD. EPA-454/B-03-001.
#'
grid.polar <- function(r, n, offset, increment, default.units="degrees", origin=c(0,0)) {
	require(sp)
	theta <- offset + increment * rep(1:n, each=length(r))
	if(max(theta) > 2 * pi && is.null(units(theta))) {
		units(theta) <- default.units
	}
	units(theta) <- "radians"	# force conversion
	x <- r * cos(theta)
	y <- r * sin(theta)
	grd <- cbind(x,y,r,theta)
	grd <- as.data.frame(grd)
	coordinates(grd) <- ~ x + y
	return(grd)
}