\name{print.regtest.rma}
\alias{print.regtest.rma}
\title{Print Method for regtest.rma Objects}
\description{
   Print method for objects of class \code{"regtest.rma"}.
}
\usage{
\method{print}{regtest.rma}(x, digits=x$digits, \dots)
}
\arguments{
	\item{x}{an object of class \code{"regtest.rma"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{\dots}{other arguments.}
}
\details{
	The output includes:
	\itemize{
	\item the model used for the regression test
	\item the predictor used for the regression test
	\item the value of the test statistic for the test that the predictor is unreleated to the outcomes
	\item the degrees of freedom of the test statistic (only if the test statistic follows a t-distribution)
	\item the p-value for the test statistic
	}
}
\value{
	The function does not return an object.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{regtest.rma}}
}
\keyword{print}
