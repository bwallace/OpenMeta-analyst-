\name{permutest}
\alias{permutest}
\title{Permutation Tests}
\description{
   The function \code{permutest} is generic. It can be used to carry out permutation tests for specific classes of objects.
}
\usage{
permutest(x, \dots)
}
\arguments{
	\item{x}{an object for which permutation tests can be carried out. See \sQuote{Details}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for objects of class \code{"rma.uni"} created by the \code{\link{rma.uni}} function. Accordingly, the corresponding method is called \code{\link{permutest.rma.uni}}. See the documentation for that function for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{permutest.rma.uni}}
}
\keyword{methods}
