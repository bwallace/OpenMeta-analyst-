\name{forest.rma}
\alias{forest.rma}
\title{Forest Plots for rma Objects}
\description{Function to create forest plots for objects of class \code{"rma"}.}
\usage{
\method{forest}{rma}(x, annotate=TRUE, addfit=TRUE, addcred=FALSE,
       xlim=NULL, alim=NULL, ylim=NULL, at=NULL, steps=5, 
       level=x$level, digits=2, refline=0, xlab=NULL, 
       slab=NULL, mlab=NULL, ilab=NULL, ilab.xpos=NULL, ilab.pos=NULL, 
       order=NULL, transf=FALSE, atransf=FALSE, targs=NULL, addrows=0, 
       efac=1, pch=15, psize=NULL, col="darkgray", border="darkgray", 
       cex=NULL, cex.lab=NULL, cex.axis=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{annotate}{logical specifying whether annotations should be added to the plot (default is \code{TRUE}).}
	\item{addfit}{logical specifying whether the summary estimate (for models without moderators) or fitted values (for models with moderators) should be added to the plot. See \sQuote{Details}.}
	\item{addcred}{logical specifying whether the bounds of the credibility interval should be added to the plot. See \sQuote{Details}.}
	\item{xlim}{horizontal limits of the plot region. Defaults to \code{NULL}, which means that the function tries to set the horizontal plot limits to some sensible values.}
	\item{alim}{the actual x axis limits. Defaults to \code{NULL}, which means that the function tries to set the x axis limits to some sensible values.}
	\item{ylim}{the y limits of the plot. Defaults to \code{NULL}, which means that the function tries to set the y axis limits to some sensible values.}
	\item{at}{position of the x axis tick marks and corresponding labels. Defaults to \code{NULL}, which means that the function tries to set the tick mark positions/labels to some sensible values.}
	\item{steps}{the number of tick marks for the x axis (default is 5). Ignored when the user specifies the positions via the \code{at} argument.}
	\item{level}{numerical value between 0 and 100 specifying the confidence interval level (the default is to take the value from the object).}
	\item{digits}{integer value specifying the number of decimal places to which the tick mark labels of the x axis and the annotations should be rounded (default is 2).}
	\item{refline}{value at which a vertical \sQuote{reference} line should be drawn (default is 0). The line can be suppressed by setting this argument to \code{NA}.}
	\item{xlab}{title for the x axis. If \code{NULL} (default), the function tries to set an appropriate axis title.}
	\item{slab}{an optional vector with unique labels for the \eqn{k} studies. Defaults to \code{NULL}, which means that the labels are either taken from the object (if study labels were specified) or simple labels are created within the function. To suppress labels, set this argument to \code{NA}.}
	\item{mlab}{an optional character string giving a label to the summary estimate from a fixed- or random-effects model. Defaults to \code{NULL}, which means that the label is created within the function.}
	\item{ilab}{an optional vector or matrix of character strings providing additional information about the studies.}
	\item{ilab.xpos}{vector of numerical value(s) specifying the x axis position(s) of the character vector(s) given via \code{ilab}.}
	\item{ilab.pos}{integer(s) (either 1, 2, 3, or 4) specifying the alignment of the character vector(s) given via \code{ilab} (2 means right, 4 mean left aligned). The default is to center the labels.}
	\item{order}{an optional character string specifying how the studies should be ordered. See \sQuote{Details}.}
	\item{transf}{an optional argument specifying the name of a function that should be used to transform the observed effect sizes, summary estimates, fitted values, and confidence interval bounds (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the x axis labels and annotations (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{transf} or \code{atransf}.}
	\item{addrows}{number of extra rows that should be added at the bottom of the forest plot (default is 0).}
	\item{efac}{vertical expansion factor for arrows, confidence interval limits, and the symbol used to denote summary estimates. The default value of 1 should usually work okay.}
	\item{pch}{plotting symbol to use for the observed effect sizes or outcomes. By default, a filled square is used. See \code{\link{points}} for other options.}
	\item{psize}{an optional vector with point sizes for the observed effect sizes or outcomes. Default is \code{NULL}, which means that the point sizes are drawn proportional to the precision of the estimates.}
	\item{cex}{an optional character and symbol expansion factor. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.lab}{an optional expansion factor for the x axis titel. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{cex.axis}{an optional expansion factor for the x axis labels. If \code{NULL} (default), the function tries to set this to a sensible value.}
	\item{col}{character string specifying the name of a color to use for the fitted values (\code{"darkgray"} by default).}
	\item{border}{character string specifying the name of a color to use for the border of the fitted values (\code{"darkgray"} by default).}
	\item{\dots}{other arguments.}
}
\details{
	The plot shows the individual observed effect sizes or outcomes with corresponding confidence intervals. 

	For fixed- and random-effects models (i.e., for models without moderators), a polygon is added to the bottom of the forest plot, showing the summary estimate based on the model (with the outer edges of the polygon indicating the confidence interval limits). For random-effects models and if \code{addcred=TRUE}, a dashed line indicates the (approximate) bounds of the credibility interval (where 95\% of the true effects are estimated to fall).

	For models involving moderators, the fitted value for each study is added as a polygon to the plot. With the \code{col} and \code{border} arguments, one can change the (border) color of these polygons.

	With the \code{transf} argument, the observed effect sizes, summary estimate, fitted values, and interval bounds can be transformed with an arbitrary function. For example, when analyzing log odds ratios, one could use \code{transf=exp} to obtain a forest plot showing the odds ratios. Alternatively, one can use the \code{atransf} argument to transform the x axis labels and annotations. The examples below illustrate the use of these arguments.

	The studies can be reordered with the \code{order} argument:
	\itemize{
	\item \code{order="obs"}: the studies are ordered by the observed effect sizes,
	\item \code{order="fit"}: the studies are ordered by the fitted values,
	\item \code{order="prec"}: the studies are ordered by their sampling variances,
	\item \code{order="resid"}: the studies are ordered by the size of their residuals,
	\item \code{order="rstandard"}: the studies are ordered by the size of their standardized residuals,
	\item \code{order="abs.resid"}: the studies are ordered by the size of their absolute residuals,
	\item \code{order="abs.rstandard"}: the studies are ordered by the size of their absolute standardized residuals.
	} Alternatively, it is possible to set \code{order} equal to a vector with indices giving the desired order.

	Additional summary estimates can also be added to the plot with the \code{\link{addpoly}} function. See the documentation for that function for examples.
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

### meta-analysis of the log relative risks using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, slab=paste(author, year),
           data=dat.bcg, measure="RR", method="REML")

### several forest plots illustrating the use of various arguments
forest(res)
forest(res, order=order(dat.bcg$ablat))
forest(res, transf=exp, alim=c(0,6), steps=4, xlim=c(-8,12), refline=1)
forest(res, atransf=exp, at=log(c(.05,.25,1,4,12)), xlim=c(-8,6), order="prec")
forest(res, slab=paste(dat.bcg$author, ", ", dat.bcg$year, sep=""), 
       xlim=c(-14,6), ylim=c(-1.5,15.5), at=log(c(.05,.25,1,4)), atransf=exp, 
       ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg), 
       ilab.xpos=c(-8.5,-7,-5,-3.5), cex=.7)
text(c(-8.5,-7,-5,-3.5), 14.5, c("TB+", "TB-", "TB+", "TB-"),  cex=.7)
text(c(-7.75,-4.25),     15.5, c("Vaccinated", "Control"),     cex=.7)
text(-14,                14.5, "Author(s) and Year",   pos=4,  cex=.7)
text(6,                  14.5, "Observed RR [95\% CI]", pos=2, cex=.7)

### mixed-effects model with absolute latitude in the model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=ablat, slab=paste(author, year),
           data=dat.bcg, measure="RR", method="REML")

### forest plots with observed and fitted values
forest(res, xlim=c(-9,5), order="fit", cex=.8, ilab=dat.bcg$ablat, 
       ilab.xpos=-4, atransf=exp, at=log(c(.05,.25,1,4)))
text(-9, 15, "Author(s) and Year",    pos=4, cex=.8)
text( 5, 15, "Observed RR [95\% CI]", pos=2, cex=.8)
text(-4, 15, "Latitude",                     cex=.8)
}
\keyword{hplot}
