\name{funnel.rma}
\alias{funnel.rma}
\title{Funnel Plots for rma Objects}
\description{Function to create funnel plots for objects of class \code{"rma"}.}
\usage{
\method{funnel}{rma}(x, xlim=NULL, ylim=NULL, xlab=NULL, ylab="Standard Error", 
       steps=5, at=NULL, atransf=FALSE, targs=NULL, 
       digits=c(2,3), level=x$level, addtau2=FALSE, 
       type="rstandard", back="lightgray", shade="white", 
       hlines="white", refline=NULL, pch=19, pch.fill=21, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{xlim}{x axis limits. Defaults to \code{NULL}, which means that the function tries to set the x axis limits to some sensible values.}
	\item{ylim}{y axis limits. Defaults to \code{NULL}, which means that the function tries to set the y axis limits to some sensible values.}
	\item{xlab}{title for the x axis. If \code{NULL} (default), the function tries to set an appropriate axis title.}
	\item{ylab}{title for the y axis.}
	\item{steps}{the number of tick marks for the y axis (default is 5).}
	\item{at}{position of the x axis tick marks and corresponding labels. Defaults to \code{NULL}, which means that the function tries to set the tick mark positions/labels to some sensible values.}
	\item{atransf}{an optional argument specifying the name of a function that should be used to transform the x axis labels (e.g., \code{transf=exp}). Defaults to \code{FALSE}, which means that no transformation is used.}
	\item{targs}{optional arguments needed by the function specified via \code{atransf}.}
	\item{digits}{integer value specifying the number of decimal places to which the tick mark labels of the x and y axis should be rounded. Can also be a vector of two integers, the first specifying the number of decimal places for the x axis, the second for the y axis labels (default is c(2,3)).}
	\item{level}{numerical value between 0 and 100 specifying the level of the pseudo confidence interval region (the default is to take the value from the object). May also be a vector of values to obtain multiple regions. See \sQuote{Examples}.}
	\item{addtau2}{logical to indicate whether the amount of heterogeneity should be accounted for when drawing the pseudo confidence interval region (default is \code{FALSE}). Ignored when the model includes moderators and residuals are plotted.}
	\item{type}{either \code{"rstandard"} (default) or \code{"rstudent"} indicating whether the usual or deleted residuals should be used in creating the funnel plot when the model involves moderators. See \sQuote{Details}.}
	\item{back}{color to use for the background of the plotting region.}
	\item{shade}{color to use for shading the pseudo confidence interval region. When \code{level} is a vector of values, different shading colors can be specified for each region.}
	\item{hlines}{color of the horizontal reference lines.}
	\item{refline}{value at which the pseudo confidence interval should be centered. Default is \code{NULL}, which means that the interval is centered at the fixed- or random-effects model estimate when the model does not include moderators and at zero when moderators are included and residuals are plotted.}
	\item{pch}{plotting symbol to use for the observed effect sizes or outcomes. By default, a solid circle is used. Can be a vector of values. See \code{\link{points}} for other options.}
	\item{pch.fill}{plotting symbol to use for the effect sizes or outcomes filled in by the trim and fill method. By default, a circle is used. Only relevant when plotting an object created by the \code{\link{trimfill}} function.}
	\item{\dots}{other arguments.}
}
\details{
	For fixed- and random-effects models (i.e., models not involving moderators), the plot shows the individual observed effect sizes or outcomes on the x axis against the corresponding standard errors (i.e., the square root of the sampling variances) on the y axis. A vertical line indicates the estimate based on the model. A pseudo confidence interval region is drawn around this value with bounds equal to \eqn{\pm 1.96 SE}{+- 1.96 SE}, where \eqn{SE} is the standard error value from the y axis. If \code{addtau2=TRUE}, then the bounds of the pseudo confidence interval region are equal to \eqn{\pm 1.96 \sqrt{SE^2 + \tau^2}}{+- 1.96 sqrt(SE^2 + tau^2)}, where \eqn{\tau^2}{tau^2} is the amount of heterogeneity as estimated by the model.

	For models involving moderators, the plot shows the residuals on the x axis against their corresponding standard errors. Either the usual or deleted residuals can be used for that purpose (set via the \code{type} argument). See \code{\link{residuals.rma}} for more details on the different types of residuals.

	If the object passed to the function comes from the \code{\link{trimfill}} function, the effect sizes or outcomes that are filled in by the trim and fill method are also added to the funnel plot.

	The arguments \code{back}, \code{shade}, and \code{hlines} can be set to \code{NULL} to suppress the shading and the horizontal reference lines.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Light, R. J. & Pillemer, D. B. (1984) \emph{Summing up: The science of reviewing research}. Cambridge, MA: Harvard University Press.

	Peters, J. L., Sutton, A. J., Jones, D. R., Abrams, K. R. & Rushton, L. (2008) Contour-enhanced meta-analysis funnel plots help distinguish publication bias from other causes of asymmetry. \emph{Journal of Clinical Epidemiology}, \bold{61}, 991--996.

	Sterne, J. A. C.  & Egger, M. (2001) Funnel plots for detecting bias in meta-analysis: Guidelines on choice of axis. \emph{Journal of Clinical Epidemiology}, \bold{54}, 1046--1055.
}
\seealso{
	\code{\link{rma.uni}}, \code{\link{rma.mh}}, \code{\link{rma.peto}}, \code{\link{influence.rma.uni}}, \code{\link{trimfill}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log relative risks using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="REML")

### standard funnel plot
funnel(res)

### funnel plot with relative risk values on the x axis
funnel(res, atransf=exp, at=log(c(.12, .25, .5, 1, 2)))

### contour-enhanced funnel plot centered at 0 (see Peters et al., 2008)
funnel(res, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), 
       cex=1.2, refline=0)

### mixed-effects model with absolute latitude in the model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=ablat,
           data=dat.bcg, measure="RR", method="REML")
funnel(res)
}
\keyword{hplot}
