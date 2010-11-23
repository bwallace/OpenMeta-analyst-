\name{round.with.zeros}
\alias{round.with.zeros}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
round.with.zeros(x, digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, digits) 
{
    y <- NULL
    for (i in 1:length(x)) {
        x.rounded <- round(x[i], digits = digits)
        numZeros <- NULL
        if (floor(x[i]) == x[i]) {
            if (digits > 0) {
                x.rounded <- paste(x.rounded, ".", sep = "")
                for (count in 1:digits) {
                  x.rounded <- paste(x.rounded, "0", sep = "")
                }
            }
        }
        else {
            pow <- 10^digits * x.rounded
            while (floor(pow) == pow) {
                pow <- pow/10
                if (floor(pow) == pow) {
                  x.rounded <- paste(x.rounded, "0", sep = "")
                }
            }
        }
        y <- c(y, toString(x.rounded))
    }
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
