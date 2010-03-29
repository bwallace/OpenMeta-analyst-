\name{plot.rma.uni}
\alias{plot.rma.uni}
\alias{plot.rma.mh}
\alias{plot.rma.peto}
\title{Plot Method for rma Objects}
\description{
   Plot method for objects of class \code{"rma.uni"}, \code{"rma.mh"}, and \code{"rma.peto"}.
}
\usage{
\method{plot}{rma.uni}(x, qqplot=FALSE, \dots)
\method{plot}{rma.mh}(x, qqplot=FALSE, \dots)
\method{plot}{rma.peto}(x, qqplot=FALSE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}, \code{"rma.mh"}, or \code{"rma.peto"}.}
	\item{qqplot}{logical indicating whether a normal QQ plot should be drawn (default is \code{FALSE}).}
	\item{\dots}{other arguments.}
}
\details{
	Four plots are produced. If the model does not contain any moderators, then a forest plot, funnel plot, radial plot, and a plot of the standardized residuals is provided. If \code{qqplot=TRUE}, the last plot is replaced by a normal QQ plot of the standardized residuals.

	If the model contains moderators, then a forest plot, funnel plot, plot of the standardized residuals against the fitted values, and a plot of the standardized residuals is provided. If \code{qqplot=TRUE}, the last plot is replaced by a normal QQ plot of the standardized residuals.
}
\note{
	If the number of studies is large, the forest plot may become quite impossible to read.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{forest}}, \code{\link{funnel}}, \code{\link{radial}}, \code{\link{qqnorm.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, 
           measure="RR", method="REML")
plot(res, qqplot=TRUE)

### mixed-effects model with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
plot(res, qqplot=TRUE)
}
\keyword{hplot}
