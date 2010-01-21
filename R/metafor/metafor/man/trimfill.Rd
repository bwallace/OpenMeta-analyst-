\name{trimfill}
\alias{trimfill}
\title{Trim and Fill Method}
\description{
   The function \code{trimfill} is generic. It can be used to apply the trim and fill method proposed by Duval and Tweedie (2000a, 2000b; see also Duval, 2005) to specific classes of objects.
}
\usage{
trimfill(x, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma.uni"}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for objects of class \code{"rma.uni"} created by the \code{\link{rma.uni}} function. Accordingly, the corresponding method is called \code{\link{trimfill.rma.uni}}. See the documentation for that function for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Duval, S. J. & Tweedie, R. L. (2000a) Trim and fill: A simple funnel-plot-based method of testing and adjusting for publication bias in meta-analysis. \emph{Biometrics}, \bold{56}, 455--463.

	Duval, S. J. & Tweedie, R. L. (2000b) A nonparametric "trim and fill" method of accounting for publication bias in meta-analysis. \emph{Journal of the American Statistical Association}, \bold{95}, 89--98.

	Duval, S. J. (2005). The trim and fill method. In H. R. Rothstein, A. J. Sutton & M. Borenstein (Eds.) (2005) \emph{Publication bias in meta-analysis: Prevention, assessment, and adjustments} (pp. 127--144). Chichester, England: Wiley.
}
\seealso{
	\code{\link{trimfill.rma.uni}}
}
\keyword{methods}
