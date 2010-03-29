\name{cint}
\alias{cint}
\title{Confidence Intervals for Model Parameters}
\description{
   The function \code{cint} is generic. It provides confidence intervals for specific classes of objects.
}
\usage{
cint(object, \dots)
}
\arguments{
	\item{object}{an object for which confidence intervals can be calculated. See \sQuote{Details}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for objects of class \code{"rma.uni"} created by the \code{\link{rma.uni}} function. Accordingly, the corresponding method is called \code{\link{cint.rma.uni}}. See the documentation for that function for more details.
}
\value{
	Confidence interval bounds of the model parameters.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{cint.rma.uni}}
}
\keyword{methods}
