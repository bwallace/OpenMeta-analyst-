\name{funnel}
\alias{funnel}
\title{Funnel Plots}
\description{
   The function \code{funnel} is generic. It can be used to create funnel plots.
}
\usage{
funnel(x, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{\dots}{other arguments.}
}
\details{
	Currently, there is only a method for handling objects of class \code{"rma"} with the \code{funnel} function. Accordingly, the corresponding method is called \code{\link{funnel.rma}}. See the documentation for that function for more details.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{funnel.rma}}
}
\keyword{hplot}
