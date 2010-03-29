\name{print.rma.uni}
\alias{print.rma.uni}
\alias{print.rma.mh}
\alias{print.rma.peto}
\alias{summary.rma}
\title{Print Methods for rma Objects}
\description{
   Print methods for objects of class \code{"rma.uni"}, \code{"rma.mh"}, and \code{"rma.peto"}.
}
\usage{
\method{print}{rma.uni}(x, digits=x$digits, showfit=FALSE, signif.legend=TRUE, \dots)
\method{print}{rma.mh}(x, digits=x$digits, showfit=FALSE, \dots)
\method{print}{rma.peto}(x, digits=x$digits, showfit=FALSE, \dots)
\method{summary}{rma}(object, digits=object$digits, showfit=TRUE, signif.legend=TRUE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}, \code{"rma.mh"}, or \code{"rma.peto"} (for \code{print}).}
	\item{object}{an object of class \code{"rma"} (for \code{summary}).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{showfit}{logical indicating whether the fit statistics and information criteria should be printed.}
	\item{signif.legend}{logical indicating whether the legend for the \sQuote{significance stars} should be printed.}
	\item{\dots}{other arguments.}
}
\details{
	The output includes:
	\itemize{
	\item the log likelihood, deviance, AIC, and BIC value (when \code{showfit=TRUE}).
	\item the amount of (residual) heterogeneity in the random/mixed-effects model (i.e., the estimate of \eqn{\tau^2}{tau^2} and its square root). Suppressed for fixed-effects models. For ML and REML estimation, the asymptotic standard error of the estimate of \eqn{\tau^2}{tau^2} is also provided.
	\item the \eqn{I^2} statistic estimates (in percent) how much of the total variability in the effect size estimates (which is composed of heterogeneity and sampling variability) can be attributed to heterogeneity among the true effects. Only supplied when fitting a random-effects model.
	\item the \eqn{H^2} statistic is the ratio of the total amount of variability in the observed outcomes to the amount of sampling variability. Only supplied when fitting a random-effects model.
	\item the results of the test for (residual) heterogeneity. This is the usual Q-test for heterogeneity when not including moderators in the model and the QE-test for residual heterogeneity when moderators are included.
	\item the results of the omnibus test of the coefficients in the model (the indices of the coefficients tested are also indicated). Suppressed if the model includes only one coefficient (e.g., only an intercept, like in the fixed- and random-effects model).
	\item a table with the estimated coefficients, corresponding standard errors, test statistics, p-values, and confidence interval bounds.
	}
	When analyzing odds ratios using the Mantel-Haenszel method, the Cochran-Mantel-Haenszel test and Tarone's test for heterogeneity are also provided.
}
\value{
	The function does not return an object.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}
}
\keyword{print}
