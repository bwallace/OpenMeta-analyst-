\name{draw.data.col}
\alias{draw.data.col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.data.col(forest.data, col, j, color.overall = "black", color.subgroup = "black", summary.line.col = "darkred", summary.line.pat = "dashed", metric = "Effect size", diam.size = 1, user.ticks = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest.data}{
%%     ~~Describe \code{forest.data} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{j}{
%%     ~~Describe \code{j} here~~
}
  \item{color.overall}{
%%     ~~Describe \code{color.overall} here~~
}
  \item{color.subgroup}{
%%     ~~Describe \code{color.subgroup} here~~
}
  \item{summary.line.col}{
%%     ~~Describe \code{summary.line.col} here~~
}
  \item{summary.line.pat}{
%%     ~~Describe \code{summary.line.pat} here~~
}
  \item{metric}{
%%     ~~Describe \code{metric} here~~
}
  \item{diam.size}{
%%     ~~Describe \code{diam.size} here~~
}
  \item{user.ticks}{
%%     ~~Describe \code{user.ticks} here~~
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
function (forest.data, col, j, color.overall = "black", color.subgroup = "black", 
    summary.line.col = "darkred", summary.line.pat = "dashed", 
    metric = "Effect size", diam.size = 1, user.ticks = NULL) 
{
    pushViewport(viewport(layout.pos.col = j, xscale = col$range))
    if (forest.data$scale == "log" && min(col$range) < 0 && max(col$range) > 
        0) 
        grid.lines(x = unit(0, "native"), y = 0:1)
    else if (forest.data$scale == "cont" && min(col$range) < 
        0 && max(col$range) > 0) 
        grid.lines(x = unit(0, "native"), y = 0:1)
    grid.lines(x = unit(col$ES[length(col$ES)], "native"), y = 0:1, 
        gp = gpar(lty = summary.line.pat, col = summary.line.col))
    if (forest.data$scale == "cont") {
        if (length(user.ticks) == 0) {
            grid.xaxis(gp = gpar(cex = 0.6))
        }
        else {
            grid.xaxis(at = user.ticks, label = user.ticks, gp = gpar(cex = 0.6))
        }
    }
    if (forest.data$scale == "log") {
        if (length(user.ticks) == 0) {
            to.make.ticks <- range(exp(col$range))
            ticks <- axTicks(1, axp = c(to.make.ticks, 3), usr = c(-100, 
                100), log = TRUE)
            log.ticks <- log(ticks)
            log.ticks <- log.ticks[log.ticks > min(col$range) - 
                0.5]
            log.ticks <- log.ticks[log.ticks < max(col$range) + 
                0.5]
            ticks <- exp(log.ticks)
        }
        else {
            log.ticks <- log(user.ticks)
            ticks <- exp(log.ticks)
        }
        grid.xaxis(at = log.ticks, label = round(ticks, 3), gp = gpar(cex = 0.6))
    }
    grid.text(metric, y = unit(-2, "lines"))
    popViewport()
    for (i in 1:length(col$rows)) {
        pushViewport(viewport(layout.pos.row = col$rows[i], layout.pos.col = j, 
            xscale = col$range))
        if (col$types[i] == 0) {
            draw.normal.CI(col$LL[i], col$ES[i], col$UL[i], col$sizes[i])
        }
        else if (col$types[i] == 1) {
            draw.summary.CI(col$LL[i], col$ES[i], col$UL[i], 
                col$sizes[i], color.subgroup, diam.size)
        }
        else if (col$types[i] == 2) {
            draw.summary.CI(col$LL[i], col$ES[i], col$UL[i], 
                col$sizes[i], color.overall, diam.size)
        }
        popViewport()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
