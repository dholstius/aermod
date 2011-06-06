#'
#' Set the "units" attribute of a vector. Implicitly promotes the vector to a quantity.
#'
#' @param x numeric vector
#' @param value character string 
#' @return vector of class "quantity" with attribute "units" set to value
#' @export
#'
'units<-.numeric' <- function(x, value) {
	stopifnot(inherits(value, "character"))
	attr(x, "units") <- value
	class(x) <- 'quantity'
	return(x)
}

#'
#' Query the "units" attribute of a vector.
#'
#' @param x vector
#' @return character string
#' @export
#'
units.numeric <- function(x) {
	attr(x, "units")
}


#'
#' Set the "units" attribute of a quantity.
#'
#' @param x numeric vector
#' @param value character string 
#' @return vector of class "quantity" with attribute "units" set to value
#' @export
#'
'units<-.quantity' <- function(x, value) {
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