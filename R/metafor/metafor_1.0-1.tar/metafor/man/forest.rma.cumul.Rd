\name{forest.cumul.rma}
\alias{forest.cumul.rma}
\title{Forest Plots for cumul.rma Objects}
\description{Function to create forest plots for objects of class \code{"cumul.rma"}.}
\usage{
\method{forest}{cumul.rma}(x, annotate=TRUE, xlim=NULL, alim=NULL, 
       ylim=NULL, at=NULL, steps=5, level=x$level, digits=2, refline=0, 
       xlab=NULL, ilab=NULL, ilab.xpos=NULL, ilab.pos=NULL, transf=FALSE, 
       atransf=FALSE, targs=NULL, addrows=0, efac=1, pch=15, psize=1, 
       cex=NULL, cex.lab=NULL, cex.axis=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"cumul.rma"}.}
	\item{annotate}{logical specifying whether annotations should be added to the plot (default is \code{TRUE}).}
	\item{xlim}{horizontal limits of the plot region. Defaults to \code{NULL}, which means that the function tries to set the horizontal plot limits to some sensible values.}
	\item{alim}{the actual x axis limits. Defaults to \code{NULL}, which means that the function tries to set the x axis limits to some sensible values.}
	\item{ylim}{the y limits of the plot. Defaults to \code{NULL}, which means that the function tries to set the y axis limits to some sensible values.}
	\item{at}{position of the x axis tick marks and corresponding labels. Defaults to \code{NULL}, which means that the function tries to set the tick mark positions/labels to some sensible values.}
	\item{steps}{the number of tick marks for the x axis (default is 5). Ignored when the user specifies the positions via the \code{at} argument.}
	\item{level}{numerical value between 0 and 100 specifying the confidence interval level (the default is to take the value from the object).}
	\item{digits}{integer value specifying the number of decimal places to which the tick mark labels of the x axis and the annotations should be rounded (default is 2).}
	\item{refline}{value at which a vertical \sQuote{reference} line should be drawn (default is 0). The line can be suppressed by setting this argument to \code{NA}.}
	\item{xlab}{title for the x axis. If \code{NULL} (default), the function tries to set an appropriate axis title.}
	\item{ilab}{an optional vector or matrix of character strings providing additional information about the studies.}
	\item{ilab.xpos}{vector of numerical value(s) specifying the x axis position(s) of the character vector(s) given via \code{ilab}.}
	\item{ilab.pos}{integer(s) (either 1, 2, 3, or 4) specifying the alignment of the character vector(s) given via \code{ilab} (2 means right, 4 mean left aligned). The default is to center the labels.}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the observed effect sizes, summary estimates, fitted values, and confidence interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the x axis labels and annotations (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{transf} or \code{atransf}.}
	\item{addrows}{number of extra rows that should be added at the bottom of the forest plot (default is 0).}
	\item{efac}{vertical expansion factor for arrows, confidence interval limits, and the symbol used to denote summary estimates. The default value of 1 should usually work okay.}
	\item{pch}{plotting symbol to use for the observed effect sizes or outcomes. By default, a filled square is used. See \code{\link{points}} for other options.}
	\item{psize}{an optional vector with point sizes for the observed effect sizes or outcomes. Default is 1.}
	\item{cex}{an optional character and symbol expansion factor. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.lab}{an optional expansion factor for the x axis titel. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.axis}{an optional expansion factor for the x axis labels. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{\dots}{other arguments.}
}
\details{
	The plot shows the estimated (average) outcome with corresponding confidence interval as one study at a time is added to the analysis.
}
\note{
	The function tries to set some sensible values for the optional arguments, but it may be necessary to tweak these in certain circumstances. In particular, if the number of studies is quite large, the labels, annotations, and symbols may become quite small and impossible to read. Stretching the plot window vertically may then provide a more readable figure (you will have to call the function again after the stretching, so that the label/symbol sizes are properly adjusted).

	If the horizontal plot and/or x axis limits are set by the user, then the horizontal plot limits (\code{xlim}) must be at least as wide as the x axis limits (\code{alim}). Moreover, the x axis limits must encompass the observed effect sizes or outcomes. These restrictions are enforced inside the function.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{forest}}, \code{\link{cumul}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log relative risks and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### random-effects model
res <- rma(yi, vi, data=dat, method="REML", slab=paste(author, ", ", year, sep=""))

x <- cumul(res, order=order(dat$year))
forest(x)

### meta-analysis of the (log) relative risks using the Mantel-Haenszel method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, 
              measure="RR", slab=paste(author, ", ", year, sep=""))
x <- cumul(res, order=order(dat$year))
forest(x)
}
\keyword{hplot}
