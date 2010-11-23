\name{extractDataLOO}
\alias{extractDataLOO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractDataLOO(results, params, title = "Leave-one-out sensitivity analysis")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
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
function (results, params, title = "Leave-one-out sensitivity analysis") 
{
    alpha <- 1 - (params$conf.level/100)
    mult <- abs(qnorm(alpha/2))
    LL <- round(exp(results$loo_results$y - mult * results$loo_results$SE), 
        digits = params$digits)
    UL <- round(exp(results$loo_results$y + mult * results$loo_results$SE), 
        digits = params$digits)
    running.summary.es <- round(exp(results$loo_results$y), digits = params$digits)
    removed.labels <- results$loo_labels
    data.to.present <- list(excluded.study = removed.labels, 
        summary.estimates = running.summary.es, lower.bound = LL, 
        upper.bound = UL)
    data.to.present
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
