\name{transf}
\alias{transf.rtoz}
\alias{transf.ztor}
\alias{transf.logit}
\alias{transf.ilogit}
\alias{transf.ztor.int}
\alias{transf.exp.int}
\title{Transformation Functions}
\description{
	A set of transformation functions useful for meta-analyses.
}
\usage{
transf.rtoz(x, \dots)
transf.ztor(x, \dots)
transf.logit(x, \dots)
transf.ilogit(x, \dots)
transf.ztor.int(x, targs, \dots)
transf.exp.int(x, targs, \dots)
}
\arguments{
	\item{x}{a vector of values to be transformed.}
	\item{targs}{a list with additional arguments for the transformation function. See \sQuote{Details}.}
	\item{\dots}{other arguments.}
}
\details{
	The following transformation functions are currently implemented:
	\itemize{
	\item \code{transf.rtoz}: Fisher's r-to-z transformation.
	\item \code{transf.ztor}: inverse of the Fisher's r-to-z transformation.
	\item \code{transf.logit}: logit (log odds) transformation.
	\item \code{transf.ilogit}: inverse of the logit transformation.
	\item \code{transf.ztor.int}: integral transformation method for the z-to-r transformation.
	\item \code{transf.exp.int}: integral transformation method for the exponential transformation.
	}
	The integral transformation method for a transformation function \eqn{h(z)} integrates \eqn{h(z) f(z)} over \eqn{z} using the limits \code{targs$lower} and \code{targs$upper}, where \eqn{f(z)} is the density of a normal distribution with mean equal to \code{x} and variance equal to \code{targs$tau2}.
}
\value{
	A vector with the transformed values.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           measure="RR", data=dat.bcg, method="REML")

### average risk ratio with 95\% CI
predict(res, transf=exp)

### average risk ratio with 95\% CI using integral transformation
predict(res, transf=transf.exp.int, targs=list(tau2=res$tau2, lower=-4, upper=4))
}
\keyword{manip}
