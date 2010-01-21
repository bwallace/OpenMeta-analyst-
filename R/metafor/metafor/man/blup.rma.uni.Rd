\name{blup.rma.uni}
\alias{blup.rma.uni}
\title{Best Linear Unbiased Predictions for rma.uni Objects}
\description{
   The function calculates the best linear unbiased predictions (BLUPs) of the true outcomes by combining the fitted values based on the fixed effects and the estimated contributions of the random effects for objects of class \code{"rma.uni"}. Corresponding standard errors and prediction interval bounds are also provided.
}
\usage{
\method{blup}{rma.uni}(x, level=x$level, digits=x$digits, transf=FALSE, targs=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}.}
	\item{level}{a numerical value between 0 and 100 specifying the prediction interval level (the default is to take the value from the object).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (the default is to take the value from the object).}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the predicted values and interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified under \code{transf}.}
	\item{\dots}{other arguments.}
}
\value{
	An object of class \code{"list.rma"}. The object is a list containing the following components:	
	\item{pred}{predicted values.}
	\item{se}{corresponding standard errors.}
	\item{pi.lb}{lower bound of the prediction intervals.}
	\item{pi.ub}{upper bound of the prediction intervals.}
	\item{\dots}{some additional elements/values.}

	The \code{"list.rma"} object is formated and printed with \code{\link{print.list.rma}}.
}
\note{
	For predicted/fitted values that are based only on the fixed effects of the model, see \code{\link{fitted.rma}} and \code{\link{predict.rma.uni}}.

	Fixed-effects models (with or without moderators) do not contain random study effects. The BLUPs for these models will therefore automatically be equal to the usual fitted values, that is, those obtained with \code{\link{fitted.rma}} and \code{\link{predict.rma.uni}}.

	When using the \code{transf} argument, the transformation is applied to the predicted values and the corresponding interval bounds. The standard errors are set equal to \code{NA}.

	The normal distribution is used to calculate the prediction intervals. When the model was fitted with the Knapp and Hartung (2003) method (i.e., \code{knha=TRUE} in the \code{\link{rma.uni}} function), then the t-distribution with \eqn{k-p} degrees of freedom is used.

	To be precise, it should be noted that the function actually calculates empirical BLUPs (EBLUPs), since the predicted values are a function of the estimated value of \eqn{\tau^2}{tau^2}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Kackar, R. N. & Harville, D. A. (1981) Unbiasedness of two-stage estimation and prediction procedures for mixed linear models. Communications in Statistics, Theory and Methods, \bold{10}, 1249--1261.

	Raudenbush, S. W. & Bryk, A. S. (1985) Empirical Bayes meta-analysis. \emph{Journal of Educational Statistics}, \bold{10}, 75--98.

	Robinson, G. K. (1991) That BLUP is a good thing: The estimation of random effects. \emph{Statistical Science}, \bold{6}, 15--32.
}
\seealso{
	\code{\link{rma.uni}}, \code{\link{predict.rma.uni}}, \code{\link{fitted.rma}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(yi, vi, data=dat, method="REML")
blup(res, transf=exp)

### illustrate shrinkage of BLUPs towards the (estimated) population average
res <- rma(yi, vi, data=dat)
blups <- blup(res)$pred
plot(NA, NA, xlim=c(.8,2.4), ylim=c(-2,0.5), pch=19, 
     xaxt="n", bty="n", xlab="", ylab="Log Relative Risk")
segments(rep(1,13), dat$yi, rep(2,13), blups, col="darkgray")
points(rep(1,13), dat$yi, pch=19)
points(rep(2,13), blups, pch=19)
axis(side=1, at=c(1,2), labels=c("Observed\nValues", "BLUPs"), lwd=0)
segments(.7, res$b, 2.15, res$b, lty="dotted")
text(2.3, res$b, expression(hat(mu)==-0.71), cex=1)
}
\keyword{models}
