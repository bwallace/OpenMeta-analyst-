\name{leave1out}
\alias{leave1out}
\title{Leave-One-Out Diagnostics}
\description{
   The function \code{leave1out} is generic. For suitable model objects, it repeatedly fits the model, leaving out one observation at a time.
}
\usage{
leave1out(x, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}, \code{"rma.mh"}, or \code{"rma.peto"}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there are methods for handling objects of class \code{"rma.uni"}, \code{"rma.mh"}, and \code{"rma.peto"} with the \code{leave1out} function. Accordingly, the corresponding methods are called \code{\link{leave1out.rma.uni}}, \code{\link{leave1out.rma.mh}}, and \code{\link{leave1out.rma.peto}}. See the documentation for those functions for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{leave1out.rma.uni}}, \code{\link{leave1out.rma.mh}}, \code{\link{leave1out.rma.peto}}
}
\keyword{methods}
