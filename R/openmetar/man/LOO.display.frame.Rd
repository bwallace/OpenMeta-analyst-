\name{LOO.display.frame}
\alias{LOO.display.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LOO.display.frame(data.to.present)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.to.present}{
%%     ~~Describe \code{data.to.present} here~~
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
function (data.to.present) 
{
    LOO.estimates <- NULL
    for (i in 1:length(data.to.present$summary.estimates)) {
        LOO.estimates[i] <- paste(data.to.present$summary.estimates[i], 
            "(", data.to.present$lower.bound[i], " to ", data.to.present$upper.bound[i], 
            ")")
    }
    estimate.title <- eval(paste("Summary estimate ", "(", params$conf.level, 
        "\% CI)", sep = ""))
    table.data <- array(data = c("Excluded study", data.to.present$excluded.study, 
        estimate.title, c(LOO.estimates)), dim = c(length(LOO.estimates) + 
        1, 2))
    table.data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
