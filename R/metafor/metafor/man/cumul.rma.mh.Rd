\name{cumul.rma.mh}
\alias{cumul.rma.mh}
\alias{cumul.rma.peto}
\title{Cumulative Meta-Analysis for rma.mh and rma.peto Objects}
\description{
	The functions \code{leave1out.rma.mh} and \code{leave1out.rma.peto} repeatedly fit the specified model, adding one observation (i.e., 2x2 table) at a time to the model.
}
\usage{
\method{cumul}{rma.mh}(x, order=NULL, digits=x$digits, transf=FALSE, \dots)
\method{cumul}{rma.peto}(x, order=NULL, digits=x$digits, transf=FALSE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.mh"} or \code{"rma.peto"}.}
	\item{order}{an optional vector with indices giving the desired order for the cumulative meta-analysis.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{transf}{logical indicating whether odds ratios or risk ratios (and the corresponding confidence interval bounds) should be given in the transformed (meaning: raw) units or in terms of log units (the default).}
	\item{\dots}{other arguments.}
}
\value{
	An object of class \code{c("list.rma", "cumul.rma")}. The object is a list containing the following components:
	\item{estimate}{estimated coefficients of the model.}
	\item{se}{standard errors of the coefficients. \code{NA} if \code{transf=TRUE}.}
	\item{zval}{test statistics of the coefficients.}
	\item{pval}{p-values for the test statistics.}
	\item{ci.lb}{lower bounds of the confidence intervals for the coefficients.}
	\item{ci.ub}{upper bounds of the confidence intervals for the coefficients.}
	\item{Q}{test statistics for the tests of heterogeneity.}
	\item{Qp}{p-values for the tests of heterogeneity.}

	The object is formated and printed with \code{\link{print.list.rma}}. A forest plot showing the results from the cumulative meta-analysis can be obtained with \code{\link{forest.cumul.rma}}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{cumul}}, \code{\link{forest.cumul.rma}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the (log) risk ratios using the Mantel-Haenszel method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR")

cumul(res, order=order(dat.bcg$year))
cumul(res, order=order(dat.bcg$year), transf=TRUE)

### meta-analysis of the (log) odds ratios using Peto's method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

cumul(res, order=order(dat.bcg$year))
cumul(res, order=order(dat.bcg$year), transf=TRUE)
}
\keyword{methods}
