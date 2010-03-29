\name{print.permutest.rma.uni}
\alias{print.permutest.rma.uni}
\title{Print Method for permutest.rma.uni Objects}
\description{
   Print method for objects of class \code{"permutest.rma.uni"}.
}
\usage{
\method{print}{permutest.rma.uni}(x, digits=x$digits, signif.legend=TRUE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"permutest.rma.uni"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{signif.legend}{logical indicating whether the legend for the \sQuote{significance stars} should be printed.}
	\item{\dots}{other arguments.}
}
\details{
	The output includes:
	\itemize{
	\item the results of the omnibus test of the coefficients in the model. Suppressed if the model includes only one coefficient (e.g., only an intercept, like in the fixed- and random-effects model). The p-value based on the permutation test is given by \code{p-val*}.
	\item a table with the estimated coefficients, corresponding standard errors, test statistics, p-values, and confidence interval bounds. The p-values based on the permutation test are given by \code{pval*}.
	}
}
\value{
	The function does not return an object.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{permutest.rma.uni}}
}
\keyword{print}
