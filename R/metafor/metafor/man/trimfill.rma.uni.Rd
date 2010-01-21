\name{trimfill.rma.uni}
\alias{trimfill.rma.uni}
\title{Trim and Fill Method for rma.uni Objects}
\description{Apply the trim and fill method to objects of class \code{"rma.uni"}.}
\usage{
\method{trimfill}{rma.uni}(x, estimator="L0", side=NULL, maxit=50, verbose=FALSE, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}.}
	\item{estimator}{either \code{"L0"} or \code{"R0"} indicating the estimator to use for estimating the number of missing studies.}
	\item{side}{either \code{"left"}, \code{"right"}, or \code{NULL} indicating on which side of the funnel plot the missing studies should be imputed. If \code{NULL}, the side is chosen within the function depending on the results of Egger's regression test (see \code{\link{regtest}} for details on this test).}
	\item{maxit}{an integer value indicating the maximum number of iterations to use for the trim and fill method.}
	\item{verbose}{logical indicating whether information about the evolution of the algorithm should be printed.}
	\item{\dots}{other arguments.}
}
\details{	
	The trim and fill method is a nonparametric (rank-based) data augmentation technique proposed by Duval and Tweedie (2000a, 2000b; see also Duval, 2005). The method can be used to estimate the number of studies missing from a meta-analysis due to the suppression of the most extreme results on one side of the funnel plot. The method then augments the observed data so that the funnel plot is more symmetric. The trim and fill method can only be used in the context of the fixed- or random-effects model (i.e., in models without moderators). The method should not be regarded as a way of yielding a more "valid" estimate of the overall effect or outcome, but as a way of examining the sensitivity of the results to one particular selection mechanism (i.e., one particular form of publication bias).
}
\value{
	An object of class \code{c("rma.uni.trimfill", "rma.uni", "rma")}. The object is a list containing the same components as objects created by \code{\link{rma.uni}}, except that the data are augmented by the trim and fill method. The object also contains a vector of dummy codes (called \code{fill}), indicating which of the data are the observed (0) and the augmented (1) data. Calling \code{\link{funnel.rma}} on the object provides a funnel plot of the observed and augmented data.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Duval, S. J. & Tweedie, R. L. (2000a) Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. \emph{Biometrics}, \bold{56}, 455--463.

	Duval, S. J. & Tweedie, R. L. (2000b) A nonparametric "trim and fill" method of accounting for publication bias in meta-analysis. \emph{Journal of the American Statistical Association}, \bold{95}, 89--98.

	Duval, S. J. (2005). The trim and fill method. In H. R. Rothstein, A. J. Sutton & M. Borenstein (Eds.) (2005) \emph{Publication bias in meta-analysis: Prevention, assessment, and adjustments} (pp. 127--144). Chichester, England: Wiley.
}
\seealso{
	\code{\link{funnel.rma}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a fixed-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="FE")
trimfill(res)
funnel(trimfill(res))

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat.bcg, measure="RR", method="REML")
trimfill(res)
funnel(trimfill(res))
}
\keyword{models}
