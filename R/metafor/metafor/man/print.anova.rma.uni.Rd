\name{print.anova.rma.uni}
\alias{print.anova.rma.uni}
\title{Print Method for anova.rma.uni Objects}
\description{
   Print method for objects of class \code{"anova.rma.uni"}.
}
\usage{
\method{print}{anova.rma.uni}(x, digits=x$digits, \dots)
}
\arguments{
	\item{x}{an object of class \code{"anova.rma.uni"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{\dots}{other arguments.}
}
\details{
	The output includes:
	\itemize{
	\item the number of parameters in the full and the reduced model.
	\item the AIC, BIC, and log likelihood of the full and the reduced model.
	\item the value of the likelihood ratio test statistic.
	\item the p-value for the likelihood ratio test.
	\item the test statistic for the test of (residual) heterogeneity for the full and the reduced model.
	\item the estimate of \eqn{\tau^2}{tau^2} from the full and the reduced model. Suppressed for fixed-effects models.
	\item the amount of (residual) heterogeneity in the reduced model that is accounted for in the full model (in percent). \code{NA} for fixed-effects models or if the amount of heterogeneity in the reduced model is equal to zero.
	}
}
\value{
	The function does not return an object.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{anova.rma.uni}}
}
\keyword{print}
