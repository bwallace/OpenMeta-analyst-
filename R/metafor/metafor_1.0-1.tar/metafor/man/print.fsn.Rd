\name{print.fsn}
\alias{print.fsn}
\title{Print Method for fsn Objects}
\description{
   Print method for objects of class \code{"fsn"}.
}
\usage{
\method{print}{fsn}(x, digits=x$digits, \dots)
}
\arguments{
	\item{x}{an object of class \code{"fsn"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{\dots}{other arguments.}
}
\details{
	The output shows the results from the fail-safe N calculation.
}
\value{
	The function does not return an object.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{fsn}}
}
\keyword{print}
