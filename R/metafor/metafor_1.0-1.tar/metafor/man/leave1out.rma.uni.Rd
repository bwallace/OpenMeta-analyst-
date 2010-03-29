\name{leave1out.rma.uni}
\alias{leave1out.rma.uni}
\title{Leave-One-Out Diagnostics for rma.uni Objects}
\description{
   The function \code{leave1out.rma.uni} repeatedly fits the specified model, leaving out one observation at a time.
}
\usage{
\method{leave1out}{rma.uni}(x, digits=x$digits, transf=FALSE, targs=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the model coefficients and interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified under \code{transf}.}
	\item{\dots}{other arguments.}
}
\details{
	The model specified by \code{x} must be a model without moderators (i.e., either a fixed- or a random-effects model).
}
\value{
	An object of class \code{"list.rma"}. The object is a list containing the following components:
	\item{estimate}{estimated coefficients of the model.}
	\item{se}{standard errors of the coefficients. \code{NA} if \code{transf} is used to transform the coefficients.}
	\item{zval}{test statistics of the coefficients.}
	\item{pval}{p-values for the test statistics.}
	\item{ci.lb}{lower bounds of the confidence intervals for the coefficients.}
	\item{ci.ub}{upper bounds of the confidence intervals for the coefficients.}
	\item{Q}{test statistics for the tests of heterogeneity.}
	\item{Qp}{p-values for the tests of heterogeneity.}
	\item{tau2}{estimated amounts of (residual) heterogeneity (only for random-effects models).}
	\item{I2}{values of \eqn{I^2} (only for random-effects models).}
	\item{H2}{values of \eqn{H^2} (only for random-effects models).}

	The \code{"list.rma"} object is formated and printed with \code{\link{print.list.rma}}.
}
\note{
	Various case diagnostics for objects of class \code{"rma.uni"} can also be obtained with the \code{\link{influence.rma.uni}} function.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{leave1out}}, \code{\link{influence.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log relative risks and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### random-effects model
res <- rma(yi, vi, data=dat, method="REML")

leave1out(res)
leave1out(res, transf=exp)
}
\keyword{methods}
