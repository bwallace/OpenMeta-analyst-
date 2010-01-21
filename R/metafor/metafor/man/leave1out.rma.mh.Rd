\name{leave1out.rma.mh}
\alias{leave1out.rma.mh}
\alias{leave1out.rma.peto}
\title{Leave-One-Out Diagnostics for rma.mh and rma.peto Objects}
\description{
   The functions \code{leave1out.rma.mh} and \code{leave1out.rma.peto} repeatedly fit the specified model, leaving out one observation (i.e., 2x2 table) at a time.
}
\usage{
\method{leave1out}{rma.mh}(x, digits=x$digits, transf=FALSE, \dots)
\method{leave1out}{rma.peto}(x, digits=x$digits, transf=FALSE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.mh"} or \code{"rma.peto"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{transf}{logical indicating whether odds ratios or risk ratios (and the corresponding confidence interval bounds) should be given in the transformed (meaning: raw) units or in terms of log units (the default).}
	\item{\dots}{other arguments.}
}
\value{
	An object of class \code{"list.rma"}. The object is a list containing the following components:
	\item{estimate}{estimated coefficients of the model.}
	\item{se}{standard errors of the coefficients. \code{NA} if \code{transf=TRUE}.}
	\item{zval}{test statistics of the coefficients.}
	\item{pval}{p-values for the test statistics.}
	\item{ci.lb}{lower bounds of the confidence intervals for the coefficients.}
	\item{ci.ub}{upper bounds of the confidence intervals for the coefficients.}
	\item{Q}{test statistics for the tests of heterogeneity.}
	\item{Qp}{p-values for the tests of heterogeneity.}

	The \code{"list.rma"} object is formated and printed with \code{\link{print.list.rma}}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{leave1out}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the (log) risk ratios using the Mantel-Haenszel method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR")

leave1out(res)
leave1out(res, transf=TRUE)

### meta-analysis of the (log) odds ratios using Peto's method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

leave1out(res)
leave1out(res, transf=TRUE)
}
\keyword{methods}
