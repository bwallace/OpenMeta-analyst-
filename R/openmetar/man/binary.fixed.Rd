\name{binary.fixed}
\Rdversion{1.1}
\alias{binary.fixed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binary.fixed(binaryData, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binaryData}{
%%     ~~Describe \code{binaryData} here~~
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
function (binaryData, params) 
{
    if (!("BinaryData" \%in\% class(binaryData))) 
        stop("Binary data expected.")
    res <<- rma.mh(ai = binaryData@g1O1, bi = binaryData@g1O2, 
        ci = binaryData@g2O1, di = binaryData@g2O2, slab = binaryData@studyNames, 
        measure = params$measure, level = params$conf.level, 
        digits = params$digits)
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest_plot <<- forest.rma(res, digits = params$digits)
    dev.off()
    images <- c(`forest plot` = forest_path)
    plot_names <- c(`forest plot` = "forest_plot")
    results <- list(images = images, summary = res, plot_names = plot_names)
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
