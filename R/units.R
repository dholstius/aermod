units.numeric <- function(x) {
	attr(x, "units")
}

`units<-.numeric` <- function(x, value) {
	stopifnot(inherits(value, "character"))
	attr(x, "units") <- value
	class(x) <- 'quantity'
	return(x)
}

`units<-.quantity` <- function(x, value) {
	stopifnot(inherits(value, "character"))
	if(units(x) != value) {
		if(units(x) == "degrees" && value == "radians") {
			scalar <- pi / 180.0
		} else if(units(x) == "radians" && value == "degrees") {
			scalar <- 180.0 / pi
		} else if(units(x) == "feet" && value == "meters") {
			scalar <- 0.3048
		} else if(units(x) == "meters" && value == "feet") {
			scalar <- 1 / 0.3048
		} else {
			stop("Can't convert from ", units(x), " to ", value)
		}
		x <- x * scalar
		attr(x, "units") <- value 
	}
	return(x)
}
