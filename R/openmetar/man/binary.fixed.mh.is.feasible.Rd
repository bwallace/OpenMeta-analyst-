\name{binary.fixed.mh.is.feasible}
\alias{binary.fixed.mh.is.feasible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine whether binary.fixed.mh can be used}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binary.fixed.mh.is.feasible(binary.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binary.data}{
%%     ~~Describe \code{binary.data} here~~
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
function (binary.data) 
{
    length(binary.data@g1O1) == length(binary.data@g1O2) && length(binary.data@g1O2) == 
        length(binary.data@g2O1) && length(binary.data@g2O1) == 
        length(binary.data@g2O2) && length(binary.data@g1O1) > 
        0
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
