\name{units.numeric}
\alias{units.numeric}
\alias{units.numeric<-}
\title{units.numeric}
\usage{
	units.numeric(x)
	units.numeric(x) <- value
}
\description{Get or set the "units" attribute of a numeric vector. If set, implicitly promotes the vector to class "quantity".}
\value{vector of class "quantity" with attribute "units" set to value}
\arguments{
	\item{x}{vector deriving from class "numeric"}
	\item{value}{character string}
}
\examples{
	x <- seq(0, 360, by=30)
	units(x) <- "degrees"
	x
	units(x) <- "radians"
	x
}