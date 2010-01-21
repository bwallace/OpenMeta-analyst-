\name{hatvalues.rma.uni}
\alias{hatvalues.rma.uni}
\title{Diagonal Elements of the Hat Matrix for rma.uni Objects}
\description{
   The function extracts the diagonal elements of the hat matrix for objects of class \code{"rma.uni"}.
}
\usage{
\method{hatvalues}{rma.uni}(model, \dots)
}
\arguments{
	\item{model}{an object of class \code{"rma.uni"}.}
	\item{\dots}{other arguments.}
}
\value{
	A vector with the diagonal elements of the hat matrix.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{rma.uni}}, \code{\link{influence.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
hatvalues(res)
}
\keyword{models}
