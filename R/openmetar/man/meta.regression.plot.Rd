\name{meta.regression.plot}
\alias{meta.regression.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Meta-Regression Plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meta.regression.plot(plot.data, outpath, symSize = 1, lcol = "darkred", metric = "Effect size", xlabel = plot.data$covariate$varname, lweight = 3, lpatern = "dotted", plotregion = "n", mcolor = "darkgreen", regline = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.data}{
%%     ~~Describe \code{plot.data} here~~
}
  \item{outpath}{
%%     ~~Describe \code{outpath} here~~
}
  \item{symSize}{
%%     ~~Describe \code{symSize} here~~
}
  \item{lcol}{
%%     ~~Describe \code{lcol} here~~
}
  \item{metric}{
%%     ~~Describe \code{metric} here~~
}
  \item{xlabel}{
%%     ~~Describe \code{xlabel} here~~
}
  \item{lweight}{
%%     ~~Describe \code{lweight} here~~
}
  \item{lpatern}{
%%     ~~Describe \code{lpatern} here~~
}
  \item{plotregion}{
%%     ~~Describe \code{plotregion} here~~
}
  \item{mcolor}{
%%     ~~Describe \code{mcolor} here~~
}
  \item{regline}{
%%     ~~Describe \code{regline} here~~
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
function (plot.data, outpath, symSize = 1, lcol = "darkred", 
    metric = "Effect size", xlabel = plot.data$covariate$varname, 
    lweight = 3, lpatern = "dotted", plotregion = "n", mcolor = "darkgreen", 
    regline = TRUE) 
{
    data.reg <- data.frame(plot.data$effects, types = plot.data$types)
    data.reg <- subset(data.reg, types == 0)
    precision = NULL
    mult = 1.96
    if (plot.data$scale == "log") {
        precision <- 1/((data.reg$UL - data.reg$LL)/(2 * mult))
    }
    else if (plot.data$scale == "cont") {
        precision <- 1/((data.reg$UL - data.reg$LL)/(2 * mult))
    }
    radii <- precision/sum(precision)
    png(file = outpath, width = 5, height = 5, units = "in", 
        res = 144)
    cov.name <- plot.data$covariate$varname
    cov.values <- plot.data$covariate$values
    if (plot.data$scale == "cont") {
        symbols(y = data.reg$ES, x = cov.values, circles = symSize * 
            radii, inches = FALSE, xlab = xlabel, ylab = metric, 
            bty = plotregion, fg = mcolor)
    }
    else {
        symbols(y = data.reg$ES, x = cov.values, circles = symSize * 
            radii, inches = FALSE, xlab = xlabel, ylab = metric, 
            bty = plotregion, fg = mcolor)
    }
    if (regline == TRUE) {
        x <- c(min(cov.values), max(cov.values))
        y <- c(plot.data$fitted.line$intercept + min(cov.values) * 
            plot.data$fitted.line$slope, plot.data$fitted.line$intercept + 
            max(cov.values) * plot.data$fitted.line$slope)
        lines(x, y, col = lcol, lwd = lweight, lty = lpatern)
    }
    graphics.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
