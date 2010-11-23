\name{plot.options}
\alias{plot.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.options(forest.data, box.sca = 1, gapSize = 3, plotWidth = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest.data}{
%%     ~~Describe \code{forest.data} here~~
}
  \item{box.sca}{
%%     ~~Describe \code{box.sca} here~~
}
  \item{gapSize}{
%%     ~~Describe \code{gapSize} here~~
}
  \item{plotWidth}{
%%     ~~Describe \code{plotWidth} here~~
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
function (forest.data, box.sca = 1, gapSize = 3, plotWidth = 4) 
{
    precision <- NULL
    effect.col.range <- NULL
    effect.col <- forest.data$effects
    if (forest.data$scale == "log") {
        precision <- sqrt(1/((effect.col$UL - effect.col$LL)/(2 * 
            1.96)))
    }
    else if (forest.data$scale == "cont") {
        precision <- sqrt(1/((effect.col$UL - effect.col$LL)/(2 * 
            1.96)))
    }
    if (min(effect.col$LL) > 0 && max(effect.col$UL) > 0) {
        effect.col.range <- c(max(0.5 * min(effect.col$ES), min(effect.col$LL)), 
            min(2 * max(effect.col$ES), max(effect.col$UL)))
    }
    else if (min(effect.col$LL) <= 0 && max(effect.col$UL) >= 
        0 && min(effect.col$ES) <= 0 && max(effect.col$ES) <= 
        0) {
        effect.col.range <- c(max(2 * min(effect.col$ES), min(effect.col$LL)), 
            min(-1.5 * max(effect.col$ES), max(effect.col$UL)))
    }
    else if (min(effect.col$LL) <= 0 && max(effect.col$UL) >= 
        0 && min(effect.col$ES) <= 0 && max(effect.col$ES) > 
        0) {
        effect.col.range <- c(max(2 * min(effect.col$ES), min(effect.col$LL)), 
            min(2 * max(effect.col$ES), max(effect.col$UL)))
    }
    else if (min(effect.col$LL) <= 0 && max(effect.col$UL) >= 
        0 && min(effect.col$ES) > 0 && max(effect.col$ES) > 0) {
        effect.col.range <- c(max(-2 * min(effect.col$ES), min(effect.col$LL)), 
            min(1.5 * max(effect.col$ES), max(effect.col$UL)))
    }
    else if (min(effect.col$LL) <= 0 && max(effect.col$UL) < 
        0) {
        effect.col.range <- c(max(2 * min(effect.col$ES), min(effect.col$LL)), 
            min(0.1 * max(effect.col$ES), max(effect.col$UL)))
    }
    merge.data <- data.frame(x = forest.data$types[-1], y = effect.col$LL, 
        z = effect.col$UL)
    merge.data <- subset(merge.data, x > 0)
    if (min(effect.col.range) >= min(merge.data$y)) 
        effect.col.range[1] <- min(merge.data$y)
    if (max(effect.col.range) <= max(merge.data$z)) 
        effect.col.range[2] <- max(merge.data$z)
    effect.col.sizes <- box.sca * precision/max(precision)
    effect.col.width <- unit(plotWidth, "inches")
    forest.params = list(col.gap = unit(gapSize, "mm"), precision = precision, 
        effect.col.range = effect.col.range, effect.col.sizes = effect.col.sizes, 
        effect.col.width = effect.col.width)
    forest.params
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
