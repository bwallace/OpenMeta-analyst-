\name{fitstats}
\alias{fitstats}
\title{Fit Statistics and Information Criteria}
\description{
   The function \code{fitstats} is generic. It extracts the log likelihood, deviance, and information criteria (e.g., AIC and BIC) for suitable objects.
}
\usage{
fitstats(x, \dots)
}
\arguments{
	\item{x}{an object for which fit statistics and information criteria can be calculated. See \sQuote{Details}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for handling objects of class \code{"rma"} with the \code{fitstats} function. Accordingly, the corresponding method is called \code{\link{fitstats.rma}}. See the documentation for that function for more details.
}
\value{
	Log likelihood, deviance, AIC, and BIC values either under the regular or restricted likelihood.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{fitstats.rma}}
}
\keyword{methods}
