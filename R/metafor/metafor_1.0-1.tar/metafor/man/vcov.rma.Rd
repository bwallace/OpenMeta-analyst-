\name{vcov.rma}
\alias{vcov.rma}
\title{Variance-Covariance Matrix of Parameter Estimates for rma Objects}
\description{
   The function extracts the (estimated) variance-covariance matrix of the fixed effects parameter estimates from objects of class \code{"rma"}.
}
\usage{
\method{vcov}{rma}(object, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\value{
	A matrix corresponding to the variance-covariance matrix.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
vcov(res)
}
\keyword{models}
