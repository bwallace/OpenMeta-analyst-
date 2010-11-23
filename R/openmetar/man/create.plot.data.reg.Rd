\name{create.plot.data.reg}
\alias{create.plot.data.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.plot.data.reg(reg.data, params, fitted.line, selected.cov = cov.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reg.data}{
%%     ~~Describe \code{reg.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{fitted.line}{
%%     ~~Describe \code{fitted.line} here~~
}
  \item{selected.cov}{
%%     ~~Describe \code{selected.cov} here~~
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
function (reg.data, params, fitted.line, selected.cov = cov.name) 
{
    scale.str <- "cont"
    if (metric.is.log.scale(params$measure)) {
        scale.str <- "log"
    }
    plot.data <- list(fitted.line = fitted.line, types = c(rep(0, 
        length(reg.data@study.names))), scale = scale.str)
    alpha <- 1 - (params$conf.level/100)
    mult <- abs(qnorm(alpha/2))
    y <- reg.data@y
    lb <- y - mult * reg.data@SE
    ub <- y + mult * reg.data@SE
    if (!is.null(selected.cov)) {
        cov.val.str <- paste("reg.data@covariates$", selected.cov, 
            sep = "")
        cov.values <- eval(parse(text = cov.val.str))
        plot.data$covariate <- list(varname = selected.cov, values = cov.values)
    }
    effects <- list(ES = y, LL = lb, UL = ub)
    plot.data$effects <- effects
    plot.data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
