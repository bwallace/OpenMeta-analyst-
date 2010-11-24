\name{print.summary.display}
\alias{print.summary.display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print Summary for Standary Meta-Anayses}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.summary.display(results, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (results, ...) 
{
    cat(results$model.title)
    cat("\n\n")
    if (!is.na(results$het.data$Title)) {
        cat(results$het.data$Title)
        cat("\n")
        print(results$het.data$het.table)
        cat("\n")
    }
    if (!is.na(results$result.data$Title)) {
        cat(results$result.data$Title)
        cat("\n")
        print(results$result.data$result.table)
        cat("\n")
    }
    if (!is.na(results$alt.data$Title)) {
        cat(results$alt.data$Title)
        cat("\n")
        print(results$alt.data$alt.table)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
