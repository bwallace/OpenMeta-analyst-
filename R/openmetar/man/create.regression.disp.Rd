\name{create.regression.disp}
\alias{create.regression.disp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.regression.disp(res, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
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
function (res, params) 
{
    coeffs <- round(res$b, digits = params$digits)
    pvals <- round.display(res$pval, digits = params$digits)
    lbs <- round(res$ci.lb, digits = params$digits)
    ubs <- round(res$ci.ub, digits = params$digits)
    reg.array <- array(c("", "Intercept", "Slope", "Estimates", 
        coeffs[1], coeffs[2], "p-Values", pvals[1], pvals[2], 
        "Lower bounds", lbs[1], lbs[2], "Upper bounds", ubs[1], 
        ubs[2]), dim = c(3, 5))
    class(reg.array) <- "table"
    reg.data <- list(Title = "", reg.table = reg.array)
    reg.disp <- list(reg.data = reg.data)
    class(reg.disp) <- "reg.display"
    return(reg.disp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
