\name{cint.rma.uni}
\alias{cint.rma.uni}
\title{Confidence Intervals for rma.uni Objects}
\description{
   The function calculates a confidence interval for the amount of (residual) heterogeneity in random- and mixed-effects models that were fitted with the \code{\link{rma.uni}} function. Confidence intervals for the model coefficients can also be obtained.
}
\usage{
\method{cint}{rma.uni}(object, fixed=FALSE, random=TRUE, level=object$level, 
     digits=object$digits, control=list(), \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma.uni"}.}
	\item{fixed}{logical indicating whether confidence intervals for the model coefficients should be returned (default is \code{FALSE}).}
	\item{random}{logical indicating whether a confidence interval for the amount of (residual) heterogeneity should be returned (default is \code{TRUE}).}
	\item{level}{a numerical value between 0 and 100 specifying the confidence interval level (the default is to take the value from the object).}
	\item{digits}{an integer specifying the number of decimal places to which the results should be rounded (the default is to take the value from the object).}
	\item{control}{a list of control values for the iterative algorithm. Defaults to an empty list, which means that default values are defined inside the function. See \sQuote{Note}.}
	\item{\dots}{other arguments.}
}
\details{
	The confidence intervals for the model coefficients are simply the usual Wald-type intervals which are also shown when printing the fitted object or when calling \code{\link{coef.rma}}.

	The confidence interval for the amount of (residual) heterogeneity is obtained iteratively via the Q-profile method as described by Hartung and Knapp (2005) and Viechtbauer (2007). The method provides an exact confidence interval for \eqn{\tau^2}{tau^2} in random- and mixed-effects models. The square root of the interval bounds is also returned for easier interpretation. For random-effects models, confidence intervals for \eqn{I^2} and \eqn{H^2} are also provided (Higgins & Thompson, 2002). Since \eqn{I^2} and \eqn{H^2} are just monotonic transformation of \eqn{\tau^2}{tau^2}, the confidence intervals for \eqn{I^2} and \eqn{H^2} are also exact.
}
\value{
	Either a single data frame or a list with two data frames (named \code{fixed} and \code{random}) with the following elements:
	\item{estimate}{estimate of the model coefficient or variance component}
	\item{ci.lb}{lower bound of the confidence interval.}
	\item{ci.ub}{upper bound of the confidence interval.}
	For fixed-effects models, the variance component estimates are \code{NA}, but the confidence interval bounds are still provided.
}
\note{
	The iterative algorithm used as part of the Q-profile method makes use of the \code{\link{uniroot}} function. By default, the desired accuracy is set equal to \code{.Machine$double.eps^0.25} and the maximum number of iterations to \code{1000}. The upper bound of the interval searched is set to 50 (which should be large enough for most cases). The desired accuracy (\code{tol}), maximum number of iterations (\code{maxiter}), and upper bound (\code{tau2.max}) can be adjusted with \code{control=list(tol=value, maxiter=value, tau2.max=value)}. One can also adjust the lower bound of the interval searched with \code{control=list(tau2.min=value)} (the default is to take that value from the object, which is 0 by default). You should only play around with this latter value if you know what you are doing.

	It is possible that the lower and upper confidence interval bounds both fall below zero (or whatever value was chosen for \code{tau2.min}). Since both values then fall outside of the parameter space, the confidence interval then just consists of the null set.

	Usually, the estimate of \eqn{\tau^2}{tau^2} from the random/mixed-effects model will fall within the confidence interval provided by the Q-profile method. However, this is not guaranteed. Depending on the method used to estimate \eqn{\tau^2}{tau^2} and the width of the confidence interval, it can happen in rare cases that the confidence interval does not actually contain the estimate (trying to explain this to reviewers can be tricky). However, uing the empirical Bayes estimator of \eqn{\tau^2}{tau^2} when fitting the model guarantees that the estimate of \eqn{\tau^2}{tau^2} falls within the confidence interval (as long as \code{level} is larger than 26).

	The Q-profile method is exact under the assumptions of the random- and mixed-effects models (i.e., normally distributed observed and true outcomes and known sampling variances). These assumptions are usually only approximately true, turning the confidence interval for \eqn{\tau^2}{tau^2} also into an approximation.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Hartung, J. & Knapp, G. (2005) On confidence intervals for the among-group variance in the one-way random effects model with unequal error variances. \emph{Journal of Statistical Planning and Inference}, \bold{127}, 157--177.

	Higgins, J. P. T. & Thompson, S. G. (2002) Quantifying heterogeneity in a meta-analysis. \emph{Statistics in Medicine}, \bold{21}, 1539--1558.

	Viechtbauer, W. (2007) Confidence intervals for the amount of heterogeneity in meta-analysis. \emph{Statistics in Medicine}, \bold{26}, 37--52.
}
\seealso{
	\code{\link{rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="REML")

### confidence interval for the total amount of heterogeneity
cint(res)

### mixed-effects model with absolute latitude in the model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=ablat,
           data=dat.bcg, measure="RR", method="REML")

### confidence interval for the residual amount of heterogeneity
cint(res)
}
\keyword{models}
