\name{predict.rma}
\alias{predict.rma}
\title{Predicted Values for rma Objects}
\description{
   The function calculates predicted values, corresponding standard errors, confidence intervals, and (approximate) credibility intervals for objects of class \code{"rma"}.
}
\usage{
\method{predict}{rma}(object, newmods=NULL, addx=FALSE, level=object$level, 
        digits=object$digits, transf=FALSE, targs=NULL, \dots)
}
\arguments{
	\item{object}{an object of class \code{"rma"}.}
	\item{newmods}{an optional vector or matrix specifying the values of the moderator values for which the predicted values should be calculated. See \sQuote{Details}.}
	\item{addx}{logical, specifying whether the values of the moderator variables should be added to the returned object. See \sQuote{Examples}.}
	\item{level}{a numerical value between 0 and 100 specifying the confidence and credibility interval level (the default is to take the value from the object).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the predicted values and interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified under \code{transf}.}
	\item{\dots}{other arguments.}
}
\details{
	For the fixed-effects model, \code{predict(object)} returns the estimated (average) outcome in the set of studies included in the meta-analysis. This is the same as the estimated intercept in the fixed-effects model.

	For the random-effects model, \code{predict(object)} returns the estimated (average) outcome in the hypothetical population of studies from which the set of studies included in the meta-analysis are assumed to be a random selection. This is the same as the estimated intercept in the random-effects model.

	For models including one or more moderators, \code{predict(object)} returns the estimated (average) outcomes for values of the moderator(s) equal to those of the \eqn{k} studies included in the meta-analysis (i.e., the fitted values for the \eqn{k} studies).

	For models including \eqn{q} moderator variables, new moderator values for \eqn{l} new studies can be specified by setting \code{newmods} equal to an \eqn{l \times q}{l x q} matrix with the corresponding new moderator values. An example is shown below.

	For random/mixed-effects models, an approximate credibility interval is also calculated. The interval estimates where \code{level} \% of the true outcomes fall in the hypothetical population of studies. Note that this interval is calculated under the assumption that the value of \eqn{\tau^2}{tau^2} is known (and not estimated). A proper method for calculating a credibility interval that accounts for the uncertainty in the estimate of \eqn{\tau^2}{tau^2} will be implemented in the future.
}
\value{
	An object of class \code{"predict.rma"}. The object is a list containing the following components:	
	\item{pred}{predicted value(s).}
	\item{se}{corresponding standard error(s).}
	\item{ci.lb}{lower bound of the confidence interval(s).}
	\item{ci.ub}{upper bound of the confidence interval(s).}
	\item{cr.lb}{lower bound of the credibility interval(s) (only random/mixed-effects models).}
	\item{cr.ub}{upper bound of the credibility interval(s) (only random/mixed-effects models).}
	\item{X}{the moderator value(s) used to calculate the predicted values (only when \code{addx=TRUE}).}
	\item{\dots}{some additional elements/values.}

	The \code{"predict.rma"} object is formated and printed with \code{\link{print.predict.rma}}.
}
\note{
	The predicted values are based only on the fixed effects of the model. Best linear unbiased predictions (BLUPs) that combine the fitted values based on the fixed effects and the estimated contributions of the random effects can be obtained with \code{\link{blup.rma.uni}} (only for objects of class \code{"rma.uni"}).

	When using the \code{transf} option, the transformation is applied to the predicted values and the corresponding interval bounds. The standard errors are set equal to \code{NA}.

	The normal distribution is used to calculate the confidence/credibility intervals. When the model was fitted with the Knapp and Hartung (2003) method (i.e., \code{knha=TRUE} in the \code{\link{rma.uni}} function), then the t-distribution with \eqn{k-p} degrees of freedom is used.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis}. San Diego, CA: Academic Press.

	Raudenbush, S. W. (2009) Analyzing effect sizes: Random effects models. In H. C. Cooper, L. V. Hedges & J. C. Valentine (Eds.), \emph{The handbook of research synthesis and meta-analysis} (2nd ed., pp. 295--315). New York: Russell Sage Foundation.
}
\seealso{
	\code{\link{fitted.rma}}, \code{\link{blup.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           measure="RR", data=dat.bcg, method="REML")

### average relative risk with 95\% CI
predict(res, transf=exp)

### mixed-effects model with absolute latitude as a moderator
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat), 
           measure="RR", data=dat.bcg, method="REML")

### predicted average relative risks for given absolute latitude values
predict(res, transf=exp, addx=TRUE)

### predicted average relative risks for 10-60 degrees absolute latitude
predict(res, newmods=c(10, 20, 30, 40, 50, 60), transf=exp)

### mixed-effects model with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year), 
           measure="RR", data=dat.bcg, method="REML")

### predicted average relative risks for 10 and 60 degrees latitude in 1950 and 1980
predict(res, newmods=cbind(c(10,60,10,60),c(1950,1950,1980,1980)), transf=exp)
}
\keyword{models}
