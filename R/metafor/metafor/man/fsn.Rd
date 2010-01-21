\name{fsn}
\alias{fsn}
\title{Fail-Safe N}
\description{Function to calculate the fail-safe N.}
\usage{
fsn(yi, vi, sei, data=NULL, type="Rosenthal", alpha=.05, 
    target=NULL, digits=4, subset=NULL)
}
\arguments{
	\item{yi}{a vector with the observed effect sizes or outcomes.}
	\item{vi}{a vector with the corresponding sampling variances.}
	\item{sei}{a vector with the corresponding standard errors.\cr(note: only one of the two, \code{vi} or \code{sei}, needs to be specified)}
	\item{data}{an optional data frame containing the variables given to the arguments above.}
	\item{type}{a vector indicating the method to use for the calculation of the fail-safe N. Possible options are \code{"Rosenthal"}, \code{"Orwin"}, or \code{"Rosenberg"}. See below for more details.}
	\item{alpha}{target alpha level to use for the Rosenthal and Rosenberg methods (.05 by default).}
	\item{target}{target average effect size to use for the Orwin method. If \code{NULL}, then the target average effect size will be equal to the observed average effect size divided by 2.}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (default is 4).}
	\item{subset}{an optional vector indicating the subset of studies that should be used for the calculation. This can be a logical vector of length \eqn{k} or a numeric vector indicating the indices of the observations to include.}
}
\details{
	The Rosenthal method calculates the number of studies averaging null results that would have to be added to the given set of observed outcomes to reduce the combined significance level (p-value) to a target alpha level (e.g., .05). The calculation is based on Stouffer's method to combine p-values and is described in Rosenthal (1979).

	The Orwin method calculates the number of studies averaging null results that would have to be added to the given set of observed outcomes to reduce the (unweighted) average effect size to a target (unweighted) average effect size. The method is described in Orwin (1983).

	The Rosenberg method calculates the number of studies averaging null results that would have to be added to the given set of observed outcomes to reduce significance level (p-value) of the (weighted) average effect size (based on a fixed-effects model) to a target alpha level (e.g., .05). The method is described in Rosenberg (2005).
}
\value{
	An object of class \code{"fsn"}. The object is a list containing the following components:
	\item{type}{the method used.}
	\item{fsnum}{the calculated fail-safe N.}
	\item{alpha}{the target alpha level.}
	\item{pval}{the p-value of the observed results. \code{NA} for the Orwin method.}
	\item{meanes}{the average effect size of the observed results. \code{NA} for the Rosenthal method.}
	\item{target}{the target effect size. \code{NA} for the Rosenthal and Rosenberg methods.}

	The results are formated and printed with the \code{\link{print.fsn}} function.
}
\note{
	For the Rosenberg method, the p-value is calculated based on a standard normal distribution (instead of a t-distribution, as suggested by Rosenberg, 2005).
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Rosenthal, R. (1979) The "file drawer problem" and tolerance for null results. \emph{Psychological Bulletin}, \bold{86}, 638--641.

	Orwin, R. G. (1983) A fail-safe N for effect size in meta-analysis. \emph{Journal of Educational Statistics}, \bold{8}, 157--159.

	Rosenberg, M. S. (2005) The file-drawer problem revisited: A general weighted method for calculating fail-safe numbers in meta-analysis. \emph{Evolution}, \bold{59}, 464--468.
}
\seealso{
	\code{\link{ranktest}}, \code{\link{trimfill}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

fsn(yi, vi, data=dat)
fsn(yi, vi, data=dat, type="Orwin")
fsn(yi, vi, data=dat, type="Rosenberg")
}
\keyword{htest}
