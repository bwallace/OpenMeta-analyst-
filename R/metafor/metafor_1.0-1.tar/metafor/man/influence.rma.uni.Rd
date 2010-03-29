\name{influence.rma.uni}
\alias{influence.rma.uni}
\title{Case Diagnostics for rma.uni Objects}
\description{
   The function calculates various case diagnostics that indicate the influence of deleting one case at a time on the model fit and the fitted/residual values for objects of class \code{"rma.uni"}.
}
\usage{
\method{influence}{rma.uni}(model, digits=model$digits, \dots)
}
\arguments{
	\item{model}{an object of class \code{"rma.uni"}.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{\dots}{other arguments.}
}
\details{
	The following leave-one-out diagnostics are calculated for each study:
	\itemize{
	\item externally standardized residual,
	\item DFFITS value,
	\item Cook's distance,
	\item covariance ratio,
	\item the leave-one-out amount of (residual) heterogeneity,
	\item the leave-one-out test statistic for the test of (residual) heterogeneity, and 
	\item DFBETAS value(s).
	} The diagonal elements of the hat matrix and the weights (in \%) given to the observed effects or outcomes during the model fitting are also provided (the hat values and weights are the same for models without moderators, but will differ once moderators are included).

	For details on externally standardized residuals, see \code{\link{rstudent.rma.uni}}.

	The DFFITS value essentially indicates how many standard deviations the predicted average effect for the \eqn{i^{th}}{ith} study changes after excluding the \eqn{i^{th}}{ith} study from the model fitting.

	Cook's distance can be interpreted as the Mahalanobis distance between the entire set of predicted values once with the \eqn{i^{th}}{ith} study included and once with the \eqn{i^{th}}{ith} study excluded from the model fitting.
	
	The covariance ratio is defined as the determinant of the variance-covariance matrix of the parameter estimates based on the dataset with the \eqn{i^{th}}{ith} study removed divided by the determinant of the variance-covariance matrix of the parameter estimates based on the complete dataset. A value below 1 therefore indicates that removal of the \eqn{i^{th}}{ith} study yields more precise estimates of the model coefficients.

	The leave-one-out amount of (residual) heterogeneity is the estimated value of \eqn{\tau^2}{tau^2} based on the dataset with the \eqn{i^{th}}{ith} study removed. Note that this is always equal to \code{0} for fixed-effects models.

	Similarly, the leave-one-out test statistic for the test of (residual) heterogeneity is the value of the test statistic of the test for (residual) heterogeneity calculated based on the dataset with the \eqn{i^{th}}{ith} study removed.

	Finally, the DFBETAS value(s) essentially indicate(s) how many standard deviations the estimated coefficient(s) change(s) after excluding the \eqn{i^{th}}{ith} study from the model fitting.
}
\value{
	An object of class \code{"infl.rma.uni"}. The object is a list containing the following components:
	\item{inf}{A data frame with columns equal to the externally standardized residuals, DFFITS values, Cook's distances, covariance ratios, leave-one-out \eqn{\tau^2}{tau^2} estimates, leave-one-out (residual) heterogeneity test statistics, hat values, and weights.}
	\item{dfb}{A data frame with columns equal to the DFBETAS values.}
	\item{\dots}{some additional elements/values.}

	The results are printed with \code{\link{print.infl.rma.uni}} and plotted with \code{\link{plot.infl.rma.uni}}.
}
\note{
	Right now, the leave-one-out diagnostics are calculated by refitting the model \eqn{k} times. Depending on how large \eqn{k} is, it may take a few moments to finish the calculations. There are shortcuts for calculating at least some of these values without refitting the model each time, but these are currently not implemented (and may not exist for all of the leave-one-out diagnostics calculated by the present function).

	It may not be possible to fit the model after deletion of the \eqn{i^{th}}{ith} study from the dataset. This will result in \code{NA} values for that study.

	Certain relationships between the leave-one-out diagnostics and the (internally or externally) standardized residuals (Belsley, Kuh, & Welsch, 1980; Cook & Weisberg, 1982) no longer hold for the meta-analytic models. Maybe there are other relationships. These remain to be determined.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Belsley, D. A., Kuh, E. & Welsch, R. E. (1980) \emph{Regression diagnostics}. New York: Wiley.

	Cook, R. D. & Weisberg, S. (1982) \emph{Residuals and influence in regression}. London: Chapman and Hall.

	Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis}. San Diego, CA: Academic Press.
}
\seealso{
	\code{\link{print.infl.rma.uni}}, \code{\link{plot.infl.rma.uni}}, \code{\link{rstudent.rma.uni}}, \code{\link{hatvalues.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
influence(res)
}
\keyword{models}
