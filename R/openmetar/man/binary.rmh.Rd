\name{binary.rmh}
\Rdversion{1.1}
\alias{binary.rmh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binary.rmh(binaryData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binaryData}{
%%     ~~Describe \code{binaryData} here~~
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
function (binaryData) 
{
    if ("BinaryData" \%in\% class(binaryData)) 
        stop("Binary data expected.")
    res <- rma.mh(binaryData@g1O1, binaryData@g1O2, binaryData@g2O1, 
        binaryData@g2O2)
    pdf("forest.pdf")
    forest.rma(res)
    dev.off()
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
