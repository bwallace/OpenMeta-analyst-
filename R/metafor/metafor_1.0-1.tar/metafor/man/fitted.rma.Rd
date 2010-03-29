\name{fitted.rma}
\alias{fitted.rma}
\title{Fitted Values for rma Objects}
\description{
   The function calculates the fitted values for objects of class \code{"rma"}.
}
\usage{
\method{fitted}{rma}(object, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\value{
	A vector with the fitted values.
}
\note{
	The \code{\link{predict.rma}} function also provides standard errors and confidence intervals for the fitted values. Best linear unbiased predictions (BLUPs) that combine the fitted values based on the fixed effects and the estimated contributions of the random effects can be obtained with \code{\link{blup.rma.uni}} (only for objects of class \code{"rma.uni"}).

	For objects not involving moderators, the fitted values are all identical to the estimated value of the model intercept.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{predict.rma}}, \code{\link{blup.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
fitted(res)
}
\keyword{models}
