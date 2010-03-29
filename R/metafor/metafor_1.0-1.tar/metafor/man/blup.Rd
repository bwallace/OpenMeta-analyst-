\name{blup}
\alias{blup}
\title{Best Linear Unbiased Predictions}
\description{
   The function \code{blup} is generic. It extracts the best linear unbiased predictions (BLUPs) for specific classes of objects. BLUPs combine the fitted values based on the fixed effects in the model and the estimated contributions of the random effects.
}
\usage{
blup(x, \dots)
}
\arguments{
	\item{x}{an object for which BLUPs are meaningful. See \sQuote{Details}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a specific method for objects of class \code{"rma.uni"} created by the \code{\link{rma.uni}} function. Accordingly, the corresponding method is called \code{\link{blup.rma.uni}}. See the documentation for that function for more details.
}
\value{
	Best linear unbiased predictions and possibly corresponding standard errors and prediction interval bounds. 
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Robinson, G. K. (1991) That BLUP is a good thing: The estimation of random effects. \emph{Statistical Science}, \bold{6}, 15--32.
}
\seealso{
	\code{\link{blup.rma.uni}}
}
\keyword{methods}
