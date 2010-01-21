\name{radial}
\alias{radial}
\alias{galbraith}
\title{Radial Plots}
\description{
   The function \code{radial} is generic. It can be used to create radial (also called Galbraith) plots.
}
\usage{
radial(x, \dots)
galbraith(x, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for handling objects of class \code{"rma"} with the \code{radial} function. Accordingly, the corresponding method is called \code{\link{radial.rma}}. See the documentation for that function for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{radial.rma}}
}
\keyword{hplot}
