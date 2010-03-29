\name{coef.rma}
\alias{coef.rma}
\title{Model Coefficients for rma Objects}
\description{
   The function extracts the estimated model coefficients, corresponding standard errors, test statistics, p-values, and confidence interval bounds from objects of class \code{"rma"}.
}
\usage{
\method{coef}{rma}(object, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\value{
	A data frame with the following elements:
	\item{estimate}{estimated model coefficient(s).}
	\item{se}{corresponding standard error(s).}
	\item{zval}{corresponding test statistic(s).}
	\item{pval}{corresponding p-value(s).}
	\item{ci.lb}{corresponding lower bound of the confidence interval(s).}
	\item{ci.ub}{corresponding upper bound of the confidence interval(s).}

	When the model was fitted with the Knapp and Hartung (2003) method (i.e., \code{knha=TRUE} in the \code{\link{rma.uni}} function), then \code{zval} is called \code{tval} in the data frame that is returned by the function.
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
coef(res)
}
\keyword{models}
