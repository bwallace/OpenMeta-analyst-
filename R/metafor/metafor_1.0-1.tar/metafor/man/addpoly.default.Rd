\name{addpoly.default}
\alias{addpoly.default}
\title{Add Polygons to Forest Plot}
\description{Function to add one or more polygons to a forest plot.}
\usage{
\method{addpoly}{default}(x, vi, sei, row=-1, level=95, digits=2, annotate=TRUE, 
        mlab=NULL, transf=FALSE, atransf=FALSE, targs=NULL, 
        col="black", efac=1, cex=NULL, \dots)
}
\arguments{
	\item{x}{a vector with the values at which the polygons should be drawn.}
	\item{vi}{a vector with the corresponding variances.}
	\item{sei}{a vector with the corresponding standard errors.\cr(note: only one of the two, \code{vi} or \code{sei}, needs to be specified)}
	\item{row}{integer specifying the (starting) row of where the polygon(s) should be placed (default is -1).}
	\item{level}{a numerical value between 0 and 100 specifying the confidence interval level (default is 95).}
	\item{digits}{integer value specifying the number of decimal places to which the annotations should be rounded (default is 2).}
	\item{annotate}{logical specifying whether annotations should be added to the plot for the polygons that are drawn (default is \code{TRUE}).}
	\item{mlab}{optional character vector with the same length as \code{x} giving labels for the polygons that are drawn. Defaults to \code{NULL}, which suppresses labels.}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the \code{x} values and confidence interval bounds. Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the annotations. Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{transf} or \code{atransf}.}
	\item{col}{color of the polygons that are drawn (default is \code{"black"}).}
	\item{efac}{vertical expansion factor for the polygons. The default value of 1 should usually work okay.}
	\item{cex}{an optional symbol expansion factor. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{\dots}{other arguments.}
}
\details{
	The function can be used to add one or more polygons to an existing forest plot created with the \code{\link{forest}} function. For example, summary estimates based on a subgrouping of the studies or from models involving moderators can be added to the plot this way. See examples below.

	The arguments \code{transf}, \code{atransf}, \code{efac}, and \code{cex} should always be set equal to the same values used to create the forest plot.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{forest.rma}}, \code{\link{forest.default}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a mixed-
### effects model with absolute latitude as a moderator
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat),
           slab=paste(author, year), data=dat.bcg, measure="RR", method="REML")

### forest plot of the observed relative risks
forest(res, addfit=FALSE, atransf=exp, addrows=3)

### predicted log average relative risks for 10, 30, and 50 degrees absolute latitude
x <- predict(res, newmods=c(10, 30, 50))

### add predicted average relative risks to forest plot
addpoly(x$pred, sei=x$se, atransf=exp, 
        mlab=c("10 Degrees", "30 Degrees", "50 Degrees"))
}
\keyword{aplot}
