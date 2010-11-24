\name{draw.summary.CI}
\alias{draw.summary.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw summary row of forest plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.summary.CI(LL, ES, UL, size, color, diam.height)
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
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{diam.height}{
%%     ~~Describe \code{diam.height} here~~
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
function (LL, ES, UL, size, color, diam.height) 
{
    grid.polygon(x = unit(c(LL, ES, UL, ES), "native"), y = unit(0.5 + 
        c(0, 0.25 * diam.height * size, 0, -0.25 * diam.height * 
            size), "npc"), gp = gpar(fill = color))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
