\name{addpoly.rma}
\alias{addpoly.rma}
\title{Add Summary Estimate Polygon to Forest Plot}
\description{Function to add a polygon to a forest plot showing the summary estimate with correspondong confidence interval based on an object of class \code{"rma"}.}
\usage{
\method{addpoly}{rma}(x, row=-2, level=x$level, digits=2, annotate=TRUE, 
        mlab=NULL, transf=FALSE, atransf=FALSE, targs=NULL, 
        col="black", efac=1, cex=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{row}{integer specifying the row number where the polygon should be placed (default is -2).}
	\item{level}{a numerical value between 0 and 100 specifying the confidence interval level (the default is to take the value from the object).}
	\item{digits}{integer value specifying the number of decimal places to which the annotations should be rounded (default is 2).}
	\item{annotate}{logical specifying whether annotations for the summary estimate should be added to the plot (default is \code{TRUE}).}
	\item{mlab}{optional character string giving a label for the summary estimate polygon. Defaults to \code{NULL}, which means that the label is created within the function.}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the summary estimate and confidence interval bound. Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the annotations. Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{transf} or \code{atransf}.}
	\item{col}{color of the polygon that is drawn (default is \code{"black"}).}
	\item{efac}{vertical expansion factor for the polygon. The default value of 1 should usually work okay.}
	\item{cex}{an optional symbol expansion factor. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{\dots}{other arguments.}
}
\details{
	The function can be used to add a polygon to an existing forest plot created with the \code{\link{forest}} function. The polygon shows the summary estimate based on a fixed- or random-effects model.
	
	The arguments \code{transf}, \code{atransf}, \code{efac}, and \code{cex} should always be set equal to the same values used to create the forest plot.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{forest.rma}}, \code{\link{forest.default}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using the Mantel-Haenszel method
res <- rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, slab=paste(author, year),
              data=dat.bcg, measure="RR")

### forest plot of the observer risk ratios with summary estimate
forest(res, atransf=exp, addrows=1, xlab="Relative Risk (log scale)")

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="REML")

### add summary estimate from the random-effects model to forest plot
addpoly(res, atransf=exp)
}
\keyword{aplot}
