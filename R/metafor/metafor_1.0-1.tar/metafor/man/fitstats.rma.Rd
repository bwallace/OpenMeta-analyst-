\name{fitstats.rma}
\alias{fitstats.rma}
\title{Fit Statistics and Information Criteria for rma Objects}
\description{
	The function extracts the log likelihood, deviance, AIC, and BIC values from objects of class \code{"rma"}.
}
\usage{
\method{fitstats}{rma}(x, REML=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{REML}{logical indicating whether the regular or restricted likelihood function should be used to obtain the fit statistics and information criteria. When \code{NULL} (default), the regular likelihood is used, unless restricted maximum likelihood estimation was used to fit the model.}
	\item{\dots}{other arguments.}
}
\value{
	A column vector with the log likelihood, deviance, AIC, and BIC values.
}
\note{
	When a random/mixed-effects model is fitted, \eqn{\tau^2}{tau^2} is counted as an additional parameter in the calculation of the AIC and BIC.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a random-effects model
res1 <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
            data=dat.bcg, measure="RR", method="ML")

### mixed-effects model with two moderators (latitude and publication year)
res2 <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
            data=dat.bcg, measure="RR", method="ML")

fitstats(res1)
fitstats(res2)
}
\keyword{models}
