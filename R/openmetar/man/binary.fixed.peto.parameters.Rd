\name{binary.fixed.peto.parameters}
\alias{binary.fixed.peto.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List parameters for binary.fixed.peto method}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binary.fixed.peto.parameters()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    apply_adjustment_to = c("only0", "all")
    params <- list(conf.level = "float", digits = "float", adjust = "float", 
        to = apply_adjustment_to)
    defaults <- list(conf.level = 95, digits = 3, adjust = 0.5)
    var_order = c("conf.level", "digits", "adjust", "to")
    parameters <- list(parameters = params, defaults = defaults, 
        var_order = var_order)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
