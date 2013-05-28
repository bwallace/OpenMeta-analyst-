\name{beta.parameter}
\alias{beta.parameter}

\title{A function that returns the shape parameters of the beta distribution}
\description{
 When provided the minimum and maximum values corresponding to the prior information on a parameter (within [0, 1]), the function returns the shape parameters of the beta distribution, namely \eqn{\alpha}{alpha} and \eqn{\beta}{beta}, that covers this range.  
}

\usage{
beta.parameter(low, up) 
}


\arguments{  
  \item{low}{numeric argument.  Lower bound of the range}
  \item{up}{numeric argument.  Upper bound of the range}
} 

\value{
It returns a list object consisting of the shape parameters \code{alpha} and \code{beta} of the Beta distribution such that \deqn{\frac{\alpha}{\alpha + \beta} = \frac{low + up}{2}}{alpha/(alpha + beta) = (low + up)/2} and \deqn{\frac{\alpha \beta}{(\alpha + \beta)^2(\alpha + \beta + 1)} = \frac{up - low}{4}}{(alpha*beta)/( (alpha + beta)^2 (alpha + beta + 1)) = (up - low)/4}

The function has been built such that the particular case where \eqn{low = up = 1}{low = up = 1} will return \eqn{\alpha = \beta = 1}{alpha = beta = 1}. 
}


\details{

The \code{low} argument must always be less than the \code{up} argument.  Also, both \code{low} and \code{up} arguments must remain within [0, 1].


}

  
\examples{
  

\dontrun{beta.parameter(-1, 0.5)} #Returns error!
\dontrun{beta.parameter(0, 0)} #Not allowed.  Returns error!
\dontrun{beta.parameter(0.75, 0.25)} #Returns error!

beta.parameter(0, 1)
beta.parameter(0.5, 1) 
beta.parameter(0.1, 0.7)            

}

\keyword{methods}

