\name{weights.rma.uni}
\alias{weights.rma.uni}
\title{Weights for rma.uni Objects}
\description{
   The function extracts the weights (in \%) given to the observed effects or outcomes during the model fitting from objects of class \code{"rma.uni"}.
}
\usage{
\method{weights}{rma.uni}(object, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma.uni"}.}
	\item{\dots}{other arguments.}
}
\value{
	A vector with the weights.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{rma.uni}}, \code{\link{influence.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
weights(res)
}
\keyword{models}
