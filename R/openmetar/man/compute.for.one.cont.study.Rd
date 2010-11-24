\name{compute.for.one.cont.study}
\alias{compute.for.one.cont.study}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute effect size and confidence bounds for one continuous study}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compute.for.one.cont.study(cont.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont.data}{
%%     ~~Describe \code{cont.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
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
function (cont.data, params) 
{
    res <- escalc(params$measure, n1i = cont.data@N1, m1i = cont.data@mean1, 
        sd1i = cont.data@sd1, n2i = cont.data@N2, m2i = cont.data@mean2, 
        sd2i = cont.data@sd2)
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
