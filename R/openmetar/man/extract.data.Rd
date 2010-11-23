\name{extract.data}
\alias{extract.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.data(binary.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binary.data}{
%%     ~~Describe \code{binary.data} here~~
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
function (binary.data, params) 
{
    alpha <- 1 - (params$conf.level/100)
    mult <- abs(qnorm(alpha/2))
    LL <- round(exp(binary.data@y - mult * binary.data@SE), digits = params$digits)
    UL <- round(exp(binary.data@y + mult * binary.data@SE), digits = params$digits)
    eventT <- round(binary.data@g1O1, digits = params$digits)
    subjectT <- round(binary.data@g1O1 + binary.data@g1O2, digits = params$digits)
    eventC <- round(binary.data@g2O1, digits = params$digits)
    subjectC <- round(binary.data@g2O1 + binary.data@g2O2, digits = params$digits)
    y <- round(binary.data@y, digits = params$digits)
    raw.data <- array(c("Study", binary.data@study.names, "Events (T)", 
        eventT, "Subjects (T)", subjectT, "Events (C)", eventC, 
        "Subjects (C)", subjectC, "Effect size", y, "Lower bound", 
        LL, "Upper bound", UL), dim = c(length(binary.data@study.names) + 
        1, 8))
    class(raw.data) <- "table"
    return(raw.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
