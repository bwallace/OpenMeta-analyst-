\name{cont.meta.regression}
\alias{cont.meta.regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cont.meta.regression(cont.data, params, cov.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont.data}{
%%     ~~Describe \code{cont.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{cov.name}{
%%     ~~Describe \code{cov.name} here~~
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
function (cont.data, params, cov.name) 
{
    res <- rma.uni(yi = cont.data@y, sei = cont.data@SE, slab = cont.data@study.names, 
        method = params$rm.method, level = params$conf.level, 
        digits = params$digits)
    betas <- res$b
    fitted.line <- list(intercept = betas[1], slope = betas[2])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
