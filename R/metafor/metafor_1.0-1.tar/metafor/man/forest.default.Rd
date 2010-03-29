\name{forest.default}
\alias{forest.default}
\title{Forest Plots}
\description{Function to create forest plots for a given set of data.}
\usage{
\method{forest}{default}(x, vi, sei, annotate=TRUE, xlim=NULL, alim=NULL, ylim=NULL, 
       at=NULL, steps=5, level=95, digits=2, refline=0, xlab=NULL, 
       slab=NULL, ilab=NULL, ilab.xpos=NULL, ilab.pos=NULL, 
       subset=NULL, transf=FALSE, atransf=FALSE, targs=NULL, 
       addrows=0, efac=1, pch=15, psize=NULL, 
       cex=NULL, cex.lab=NULL, cex.axis=NULL, \dots)
}
\arguments{
	\item{x}{a vector of length \eqn{k} with the observed effect sizes or outcomes.}
	\item{vi}{a vector of length \eqn{k} with the corresponding sampling variances.}
	\item{sei}{a vector of length \eqn{k} with the corresponding standard errors.\cr(note: only one of the two, \code{vi} or \code{sei}, needs to be specified)}
	\item{annotate}{logical specifying whether annotations should be added to the plot (default is \code{TRUE}).}
	\item{xlim}{horizontal limits of the plot region. Defaults to \code{NULL}, which means that the function tries to set the horizontal plot limits to some sensible values.}
	\item{alim}{the actual x axis limits. Defaults to \code{NULL}, which means that the function tries to set the x axis limits to some sensible values.}
	\item{ylim}{the y limits of the plot. Defaults to \code{NULL}, which means that the function tries to set the y axis limits to some sensible values.}
	\item{at}{position of the x axis tick marks and corresponding labels. Defaults to \code{NULL}, which means that the function tries to set the tick mark positions/labels to some sensible values.}
	\item{steps}{the number of tick marks for the x axis (default is 5). Ignored when the user specifies the positions via the \code{at} argument.}
	\item{level}{numerical value between 0 and 100 specifying the confidence interval level (default is 95).}
	\item{digits}{integer value specifying the number of decimal places to which the tick mark labels of the x axis and the annotations should be rounded (default is 2).}
	\item{refline}{value at which a vertical \sQuote{reference} line should be drawn (default is 0). The line can be suppressed by setting this argument to \code{NA}.}
	\item{xlab}{title for the x axis. If \code{NULL} (default), the function tries to set an appropriate axis title.}
	\item{slab}{an optional vector with unique labels for the \eqn{k} studies. Defaults to \code{NULL}, which means that simple labels are created within the function. To suppress labels, set this argument to \code{NA}.}
	\item{ilab}{an optional vector or matrix of character strings providing additional information about the studies.}
	\item{ilab.xpos}{numerical value(s) specifying the x axis position(s) of the character vector(s) given via \code{ilab}.}
	\item{ilab.pos}{integer(s) (either 1, 2, 3, or 4) specifying the alignment of the character vector(s) given via \code{ilab} (2 means right, 4 mean left aligned). The default is to center the labels.}
	\item{subset}{an optional vector indicating the subset of studies that should be used for the plot. This can be a logical vector of length \eqn{k} or a numeric vector indicating the indices of the observations to include. Note that this argument can also be used for reordering the studies.}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the observed effect sizes and corresponding confidence interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the x axis labels and annotations (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{transf} or \code{atransf}.}
	\item{addrows}{number of extra rows that should be added at the bottom of the forest plot (default is 0).}
	\item{efac}{vertical expansion factor for arrows, confidence interval limits, and the symbol used to denote summary estimates. The default value of 1 should usually work okay.}
	\item{pch}{plotting symbol to use for the observed effect sizes or outcomes. By default, a filled square is used. See \code{\link{points}} for other options.}
	\item{psize}{an optional vector with point sizes for the observed effect sizes or outcomes. Default is \code{NULL}, which means that the point sizes are drawn proportional to the precision of the estimates.}
	\item{cex}{an optional character and symbol expansion factor. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.lab}{an optional expansion factor for the x axis titel. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.axis}{an optional expansion factor for the x axis labels. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{\dots}{other arguments.}
}
\details{
	The plot shows the individual observed effect sizes or outcomes with corresponding confidence intervals.

	With the \code{transf} argument, the observed effect sizes and corresponding confidence interval bounds can be transformed with an arbitrary function. For example, if log odds ratios are provided to the function, then one could use \code{transf=exp} to obtain a forest plot showing the odds ratios. Alternatively, one can use the \code{atransf} argument to transform the x axis labels and annotations. The examples below illustrate the use of these arguments.

	Summary estimates can also be added to the plot with the \code{\link{addpoly}} function. See the documentation for that function for examples.
}
\note{
	The function tries to set some sensible values for the optional arguments, but it may be necessary to tweak these in certain circumstances. In particular, if the number of studies is quite large, the labels, annotations, and symbols may become quite small and impossible to read. Stretching the plot window vertically may then provide a more readable figure (you will have to call the function again after the stretching, so that the label/symbol sizes are properly adjusted).

	If the horizontal plot and/or x axis limits are set by the user, then the horizontal plot limits (\code{xlim}) must be at least as wide as the x axis limits (\code{alim}). Moreover, the x axis limits must encompass the observed effect sizes or outcomes. These restrictions are enforced inside the function.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{forest}}, \code{\link{addpoly}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log relative risks and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### forest plot of the observed relative risks
forest(dat$yi, dat$vi, slab=paste(dat.bcg$author, dat$year), atransf=exp)
}
\keyword{hplot}
