\name{plot.infl.rma.uni}
\alias{plot.infl.rma.uni}
\title{Plot Method for infl.rma.uni Objects}
\description{
   Plot method for objects of class \code{"infl.rma.uni"}.
}
\usage{
\method{plot}{infl.rma.uni}(x, plotdfb=FALSE, dfbnew=FALSE, logcov=TRUE, las=0,
     pch=21, bg="black", bg.infl="red", col.na="lightgray", \dots)
}
\arguments{
	\item{x}{an object of class \code{"infl.rma.uni"}.}
	\item{plotdfb}{logical indicating whether the DFBETAS values should be plotted (default is \code{FALSE}).}
	\item{dfbnew}{logical indicating whether a new device should be opened for plotting the DFBETAS values (default is \code{FALSE}). If set to \code{TRUE}, then \code{plotdfb} is automatically set to \code{TRUE} as well.}
	\item{logcov}{logical indicating whether the covariance ratios should be plotted on a log scale (default is \code{FALSE}).}
	\item{las}{numeric (\code{0,1,2,3}). Defines style of axis labels (see \code{\link{par}}). The most useful alternative to 0 is 3, so that the x axis labels are drawn vertical to the axis.}
	\item{pch}{plotting symbol to use. By default, a filled circle is used. See \code{\link{points}} for other options.}
	\item{bg}{color to use for filling the plotting symbol (default is \code{"black"}).}
	\item{bg.infl}{color to use for filling the plotting symbol when the point is considered influential (default is \code{"red"}).}
	\item{col.na}{color to use for lines connecting two points with NAs in between (default is \code{"lightgray"}).}
	\item{\dots}{other arguments.}
}
\details{
	The function plots the externally standardized residuals, DFFITS values, Cook's distances, covariance ratios, leave-one-out \eqn{\tau^2}{tau^2} estimates, leave-one-out (residual) heterogeneity test statistics, hat values, and weights. If \code{plotdfb=TRUE}, the DFBETAS values are also plotted either after confirming the page change (if \code{newdfb=FALSE}) or on a separate device (if \code{newdfb=TRUE}). Points are considered \sQuote{influential} if at least one of the following is true:
	\itemize{
	\item The absolute DFFITS value is larger than \eqn{3\sqrt{p/(k-p)}}{3sqrt(p/(k-p))}, where \eqn{p} is the number of model coefficients and \eqn{k} the number of studies.
	\item The lower tail area of a chi-square distribution with \eqn{p} degrees of freedom cut off by the Cook's distance is larger than 50\%.
	\item The hat value is larger than \eqn{3(p/k)}{3(p/k)}.
	\item Any DFBETAS value is larger than \eqn{1}.
	}

	These cut-offs are also indicated in the plot with horizontal reference lines. In addition, on the plot of the externally standardized residuals, horizontal reference lines are drawn at -1.96, 0, and 1.96. On the plot of the hat values, a horizontal reference line is drawn at \eqn{p/k}. Since the sum of the hat values is equal to \eqn{p}, the value \eqn{p/k} indicates equal hat values for all \eqn{k} studies. Finally, on the plot of weights, a horizontal reference line is drawn at \eqn{100/k}, corresponding to the value for equal weights (in \%) for all \eqn{k} studies. Note that all weights will automatically be equal to each other when using unweighted model fitting. Also, except for their scaling, the hat values and weights will be equal to each other in models without moderators.

	The chosen cut-offs are (somewhat) arbitrary. Substantively informed judgment should always be used when examining the influence of each study on the results.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{influence.rma.uni}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a mixed-effects model
### with two moderators (absolute latitude and publication year)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year),
           data=dat.bcg, measure="RR", method="REML")
plot(influence(res))
plot(influence(res), plotdfb=TRUE)
}
\keyword{hplot}
