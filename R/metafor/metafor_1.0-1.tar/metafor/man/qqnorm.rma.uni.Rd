\name{qqnorm.rma.uni}
\alias{qqnorm.rma.uni}
\alias{qqnorm.rma.mh}
\alias{qqnorm.rma.peto}
\title{Normal QQ Plot for rma Objects}
\description{Function to create normal QQ plots for objects of class \code{"rma.uni"}, \code{"rma.mh"}, and \code{"rma.peto"}.}
\usage{
\method{qqnorm}{rma.uni}(y, type="rstandard", pch=19, envelope=TRUE, level=y$level, 
       bonferroni=FALSE, reps=1000, smooth=TRUE, bass=0, \dots)
\method{qqnorm}{rma.mh}(y, type="rstandard", pch=19, \dots)
\method{qqnorm}{rma.peto}(y, type="rstandard", pch=19, \dots)
}
\arguments{
	\item{y}{an object of class \code{"rma.uni"}, \code{"rma.mh"}, or \code{"rma.peto"}.}
	\item{type}{either \code{"rstandard"} (default) or \code{"rstudent"} indicating whether the usual or deleted residuals should be used in creating the plot. See \sQuote{Details}.}
	\item{pch}{plotting symbol to use for the observed effect sizes or outcomes. By default, a solid circle is used. See \code{\link{points}} for other options.}
	\item{envelope}{logical indicating whether a pseudo confidence envelope should be simulated and added to the plot (default is \code{TRUE})). Only for objects of class \code{"rma.uni"}. See \sQuote{Details}.}
	\item{level}{numerical value between 0 and 100 specifying the level of the pseudo confidence envelope (the default is to take the value from the object).}
	\item{bonferroni}{logical indicating whether the bounds of the envelope should be Bonferroni corrected.}
	\item{reps}{numerical value indicating the number of iterations to use for simulating the pseudo confidence envelope (default is 1000).}
	\item{smooth}{logical indicating whether the results from the simulation should be smoothed (default is \code{TRUE}).}
	\item{bass}{numerical value that controls the degree of smoothing (default is 0).}
	\item{\dots}{other arguments.}
}
\details{
	The plot shows the theoretical quantiles of a normal distribution on the horizontal axis against the observed quantiles for either the standardized residuals (\code{type="rstandard"}, the default) or the externally standardized residuals (\code{type="rstudent"}) on the vertical axis.

	For reference, a line is added to the plot with slope of 1, going through the (0,0) point.

	For objects of class \code{"rma.uni"}, it is also possible to add a pseudo confidence envelope to the plot. The envelope is created based on the quantiles of sets of pseudo residuals simulated from the given model (for details, see Cook & Weisberg, 1982). The number of sets simulated can be controlled with the \code{reps} argument. When \code{smooth=TRUE}, the simulated bounds are smoothed with Friedman's SuperSmoother (see \code{\link{supsmu}}). The \code{bass} argument can be set to a number between 0 and 10, with higher numbers indicating increasing smoothness. If \code{bonferroni=TRUE}, the envelope bounds are Bonferroni corrected, so that the envelope can be regarded as a confidence region for all \eqn{k} residuals simultaneously. The default however is \code{bonferroni=FALSE}, which makes the plot more sensitive to deviations from normality.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Cook, R. D. & Weisberg, S. (1982) \emph{Residuals and influence in regression}. London: Chapman and Hall.
}
\seealso{
	\code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="REML")
qqnorm(res)

### mixed-effects model with absolute latitude as a moderator
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat), 
           measure="RR", data=dat.bcg, method="REML")
qqnorm(res)
}
\keyword{hplot}
