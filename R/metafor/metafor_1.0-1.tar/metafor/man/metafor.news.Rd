\name{metafor.news}
\alias{metafor.news}
\title{Read News File of the Metafor Package}
\description{
   Read news file of the \pkg{\link{metafor-package}} package.
}
\usage{
metafor.news(\dots)
}
\arguments{
  \item{\dots}{arguments passed to \code{\link{file.show}}.}
}
\details{
	The function just makes use of the \code{\link{file.show}} command to display the \file{NEWS} file of the package.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\seealso{
	\code{\link{file.show}}
}
\examples{
metafor.news() 
}
\keyword{IO}
