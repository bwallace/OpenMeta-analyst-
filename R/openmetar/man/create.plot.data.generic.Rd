\name{create.plot.data.generic}
\alias{create.plot.data.generic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.plot.data.generic(om.data, params, res, selected.cov = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{om.data}{
%%     ~~Describe \code{om.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
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
function (om.data, params, res, selected.cov = NULL) 
{
    scale.str <- "cont"
    if (metric.is.log.scale(params$measure)) {
        scale.str <- "log"
    }
    plot.data <- list(label = c(paste(params$fp_col1_str, sep = ""), 
        om.data@study.names, "Overall"), types = c(3, rep(0, 
        length(om.data@study.names)), 2), scale = scale.str)
    alpha <- 1 - (params$conf.level/100)
    mult <- abs(qnorm(alpha/2))
    y <- om.data@y
    lb <- y - mult * om.data@SE
    ub <- y + mult * om.data@SE
    y.overall <- res$b[1]
    lb.overall <- res$ci.lb[1]
    ub.overall <- res$ci.ub[1]
    y.rounded <- binary.transform.f(params$measure)$display.scale(y)
    y.rounded <- round.with.zeros(y.rounded, params$digits)
    lb.rounded <- binary.transform.f(params$measure)$display.scale(lb)
    lb.rounded <- round.with.zeros(lb.rounded, params$digits)
    ub.rounded <- binary.transform.f(params$measure)$display.scale(ub)
    ub.rounded <- round.with.zeros(ub.rounded, params$digits)
    y.overall.rounded <- binary.transform.f(params$measure)$display.scale(y.overall)
    y.overall.rounded <- round.with.zeros(y.overall.rounded, 
        params$digits)
    lb.overall.rounded <- binary.transform.f(params$measure)$display.scale(lb.overall)
    lb.overall.rounded <- round.with.zeros(lb.overall.rounded, 
        params$digits)
    ub.overall.rounded <- binary.transform.f(params$measure)$display.scale(ub.overall)
    ub.overall.rounded <- round.with.zeros(ub.overall.rounded, 
        params$digits)
    if (params$fp_show_col2 == "TRUE") {
        additional.cols <- list(es = c(paste(params$fp_col2_str, 
            sep = ""), paste(y.rounded, " (", lb.rounded, " , ", 
            ub.rounded, ")", sep = ""), paste(y.overall.rounded, 
            " (", lb.overall.rounded, " , ", ub.overall.rounded, 
            ")", sep = "")))
        plot.data$additional.col.data <- additional.cols
    }
    effects <- list(ES = c(y, y.overall), LL = c(lb, lb.overall), 
        UL = c(ub, ub.overall))
    plot.data$effects <- effects
    if (!is.null(selected.cov)) {
        cov.val.str <- paste("om.data@covariates$", selected.cov, 
            sep = "")
        cov.values <- eval(parse(text = cov.val.str))
        plot.data$covariate <- list(varname = selected.cov, values = cov.values)
    }
    plot.data$fp_xlabel <- paste(params$fp_xlabel, sep = "")
    plot.data$fp_xticks <- params$fp_xticks
    plot.data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
