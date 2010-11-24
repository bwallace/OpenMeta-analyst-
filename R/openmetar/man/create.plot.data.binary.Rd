\name{create.plot.data.binary}
\alias{create.plot.data.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create plot data for binary methods forest plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.plot.data.binary(binary.data, params, res, selected.cov = NULL, include.overall = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binary.data}{
%%     ~~Describe \code{binary.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{selected.cov}{
%%     ~~Describe \code{selected.cov} here~~
}
  \item{include.overall}{
%%     ~~Describe \code{include.overall} here~~
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
function (binary.data, params, res, selected.cov = NULL, include.overall = TRUE) 
{
    plot.data <- create.plot.data.generic(binary.data, params, 
        res, selected.cov = selected.cov)
    if ((length(binary.data@g1O1) > 0) && (params$fp_show_col3 == 
        "TRUE")) {
        plot.data$additional.col.data$cases = c(paste(params$fp_col3_str, 
            sep = ""), paste(binary.data@g1O1, " / ", binary.data@g1O1 + 
            binary.data@g1O2, sep = ""), paste(sum(binary.data@g1O1), 
            " / ", sum(binary.data@g1O1 + binary.data@g1O2), 
            sep = ""))
    }
    if ((length(binary.data@g2O1) > 0) && (params$fp_show_col4 == 
        "TRUE")) {
        plot.data$additional.col.data$controls = c(paste(params$fp_col4_str, 
            sep = ""), paste(binary.data@g2O1, " / ", binary.data@g1O1 + 
            binary.data@g2O2, sep = ""), paste(sum(binary.data@g2O1), 
            " / ", sum(binary.data@g1O1 + binary.data@g2O2), 
            sep = ""))
    }
    plot.data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
