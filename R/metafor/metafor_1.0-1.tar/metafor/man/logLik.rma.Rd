\name{logLik.rma}
\alias{logLik.rma}
\title{Log-Likelihood of rma Objects}
\description{
   The function extracts the (restricted) log likelihood for objects of class \code{"rma"}.
}
\usage{
\method{logLik}{rma}(object, REML=NULL, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma"}.}
	\item{REML}{logical indicating whether the regular or restricted log likelihood should be returned. When \code{NULL} (default), the regular log likelihood is returned, unless restricted maximum likelihood estimation was used to fit the model.}
	\item{\dots}{other arguments.}
}
\value{
	The (restricted) log likelihood of the model evaluated at the estimated coefficient(s).
}
\note{
	For a more complete set of fit statistics and information criteria, see \code{\link{fitstats.rma}}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{fitstats.rma}}, \code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
logLik(res)
}
\keyword{models}
