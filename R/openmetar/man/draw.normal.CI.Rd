\name{draw.normal.CI}
\alias{draw.normal.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw confidence intervals in forest plots}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.normal.CI(LL, ES, UL, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LL}{
%%     ~~Describe \code{LL} here~~
}
  \item{ES}{
%%     ~~Describe \code{ES} here~~
}
  \item{UL}{
%%     ~~Describe \code{UL} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
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
function (LL, ES, UL, size) 
{
    grid.rect(x = unit(ES, "native"), width = unit(size, "snpc"), 
        height = unit(size, "snpc"), gp = gpar(fill = "black"))
    if (convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 
        1 && convertX(unit(LL, "native"), "npc", valueOnly = TRUE) > 
        0) {
        grid.arrows(x = unit(c(LL, 1), c("native", "npc")), length = unit(0.05, 
            "inches"))
    }
    else if (convertX(unit(UL, "native"), "npc", valueOnly = TRUE) < 
        1 && convertX(unit(LL, "native"), "npc", valueOnly = TRUE) < 
        0) {
        grid.arrows(x = unit(c(UL, 0), c("native", "npc")), length = unit(0.05, 
            "inches"))
    }
    else if (convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 
        1 && convertX(unit(LL, "native"), "npc", valueOnly = TRUE) < 
        0) {
        grid.arrows(x = unit(c(ES, 0), c("native", "npc")), length = unit(0.05, 
            "inches"))
        grid.arrows(x = unit(c(ES, 1), c("native", "npc")), length = unit(0.05, 
            "inches"))
    }
    else {
        line.col <- if ((convertX(unit(ES, "native") + unit(0.5 * 
            size, "lines"), "native", valueOnly = TRUE) > UL) && 
            (convertX(unit(ES, "native") - unit(0.5 * size, "lines"), 
                "native", valueOnly = TRUE) < LL)) 
            "white"
        else "black"
        grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, gp = gpar(col = line.col))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
