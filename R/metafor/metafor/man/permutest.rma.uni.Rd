\name{permutest.rma.uni}
\alias{permutest.rma.uni}
\title{Permutation Tests for rma.uni Objects}
\description{
   The function carries out permutation tests for objects of class \code{"rma.uni"}.
}
\usage{
\method{permutest}{rma.uni}(x, iter=1000, progbar=TRUE, digits=x$digits, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}.}
	\item{iter}{integer to specify the number of iterations for the permutation test (default is 1000 iterations).}
	\item{progbar}{logical indicating whether a progress bar should be shown (default is \code{TRUE}).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{\dots}{other arguments.}
}
\details{
	For models without moderators, the permutation test is carried out by permuting the sign of the observed effect sizes or outcomes. The p-value of the permutation test is then the proportion of times that the test statistic under the permuted data is more extreme than under the actually observed data. See Follmann and Proschan (1999) for more details.

	For models with moderators, the permutation test is carried out by permuting the rows of the design matrix. The p-value for a particular model coefficient is equal to the proportion of times that the test statistic for the coefficient under the permuted data is more extreme than under the actually observed data. Similarly, for the omnibus test, the p-value is the proportion of times that the test statistic for the omnibus test is more extreme than the actually observed one. See Higgins and Thompson (2004) for more details.
}
\value{
	An object of class \code{"permutest.rma.uni"}. The object is a list containing the following components:	
	\item{pval}{p-value(s) based on the permutation test.}
	\item{QMp}{p-value for the omnibus test of coefficients based on the permutation test.}
	\item{\dots}{some additional elements/values are passed on.}

	The results are formated and printed with the \code{\link{print.permutest.rma.uni}} function.
}
\note{
	An exact permutation test would require fitting the model to each possible permutation once. However, the number of possible permutations increases rapidly with the number of outcomes/studies (i.e., \eqn{k}). For example, for \eqn{k=5}, there are only 120 possible permutations. For \eqn{k=8}, there are already 40,320. And for \eqn{k=10}, there are 3,628,800. Therefore, going through all possible permutations quickly becomes infeasible. Instead, the function approximates the exact permutation-based p-values by going through a smaller number (as specified by the \code{iter} argument) of random permutations. Therefore, running the function twice on the same data will yield slightly different p-values. Setting \code{iter} sufficiently large ensures that the results become stable.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Follmann, D. A. & Proschan, M. A. (1999) Valid inference in random effects meta-analysis. \emph{Biometrics}, \bold{55}, 732--737.

	Higgins, J. P. T. & Thompson, S. G. (2004) Controlling the risk of spurious findings from meta-regression. \emph{Statistics in Medicine}, \bold{23}, 1663--1682.
}
\seealso{
	\code{\link{rma.uni}}, \code{\link{print.permutest.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### random-effects model 
res <- rma(yi, vi, data=dat, method="REML")
\dontrun{permutest(res)}

### mixed-effects model with two moderators (absolute latitude and publication year)
res1 <- rma(yi, vi, mods=cbind(ablat, year), data=dat, method="REML")
\dontrun{permutest(res)}
}
\keyword{models}
