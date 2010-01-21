\name{regtest}
\alias{regtest}
\title{Regression Tests for Funnel Plot Asymmetry}
\description{
   The function \code{regtest} is generic. It can be used to carry out various tests for funnel plot asymmetry, including Egger's regression test and variations thereof.
}
\usage{
regtest(x, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for handling objects of class \code{"rma"} with the \code{regtest} function. Accordingly, the corresponding method is called \code{\link{regtest.rma}}. See the documentation for that function for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{regtest.rma}}
}
\keyword{htest}
