\name{get.res.for.one.cont.study}
\alias{get.res.for.one.cont.study}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Residuals for One Continuous Study}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.res.for.one.cont.study(cont.data, params)
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
    y <- NULL
    se <- NULL
    if (length(cont.data@y) == 0 || is.na(cont.data@y)) {
        res <- compute.for.one.cont.study(cont.data, params)
        y <- res$yi[1]
        se <- sqrt(res$vi[1])
    }
    else {
        y <- cont.data@y[1]
        se <- cont.data@SE[1]
    }
    alpha <- 1 - (params$conf.level/100)
    mult <- abs(qnorm(alpha/2))
    ub <- y + mult * se
    lb <- y - mult * se
    res <- list(b = c(y), ci.lb = lb, ci.ub = ub)
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
