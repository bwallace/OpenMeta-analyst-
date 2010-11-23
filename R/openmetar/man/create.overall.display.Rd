\name{create.overall.display}
\alias{create.overall.display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.overall.display(res, study.names, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{study.names}{
%%     ~~Describe \code{study.names} here~~
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
function (res, study.names, params) 
{
    res
    params$digits
    res <- round(res, digits = params$digits)
    overall.array <- array(c("", study.names, "Estimates", res[, 
        1], "Lower bounds", res[, 2], "Upper bounds", res[, 3]), 
        dim = c(length(study.names) + 1, 4))
    class(overall.array) <- "table"
    overall.data <- list(Title = "", overall.table = overall.array)
    overall.disp <- list(overall.data = overall.data)
    class(overall.disp) <- "overall.display"
    return(overall.disp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
