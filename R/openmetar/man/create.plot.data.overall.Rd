\name{create.plot.data.overall}
\alias{create.plot.data.overall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create plot data for cumulative and leave-one-out forest plots}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.plot.data.overall(params, res, study.names, addRow1Space, selected.cov = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{study.names}{
%%     ~~Describe \code{study.names} here~~
}
  \item{addRow1Space}{
%%     ~~Describe \code{addRow1Space} here~~
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
function (params, res, study.names, addRow1Space, selected.cov = NULL) 
{
    scale.str <- "cont"
    if (metric.is.log.scale(params$measure)) {
        scale.str <- "log"
    }
    if (addRow1Space == TRUE) {
        study.names[1] <- paste("   ", study.names[1], sep = "")
    }
    plot.data <- list(label = c("Studies", study.names), types = c(3, 
        rep(0, length(study.names))), scale = scale.str)
    y <- res[, 1]
    lb <- res[, 2]
    ub <- res[, 3]
    y.rounded <- binary.transform.f(params$measure)$display.scale(y)
    lb.rounded <- binary.transform.f(params$measure)$display.scale(lb)
    ub.rounded <- binary.transform.f(params$measure)$display.scale(ub)
    y.rounded <- round.with.zeros(y.rounded, params$digits)
    lb.rounded <- round.with.zeros(lb.rounded, params$digits)
    ub.rounded <- round.with.zeros(ub.rounded, params$digits)
    if (params$fp_show_col2 == "TRUE") {
        additional.cols <- list(es = c(paste(params$fp_col2_str, 
            sep = ""), paste(y.rounded, " (", lb.rounded, " , ", 
            ub.rounded, ")", sep = "")))
        plot.data$additional.col.data <- additional.cols
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
