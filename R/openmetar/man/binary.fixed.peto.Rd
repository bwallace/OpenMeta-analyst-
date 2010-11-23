\name{binary.fixed.peto}
\alias{binary.fixed.peto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binary.fixed.peto(binary.data, params)
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
    if (!("BinaryData" \%in\% class(binary.data))) 
        stop("Binary data expected.")
    if (length(binary.data@g1O1) == 1) {
        res <- get.res.for.one.binary.study(binary.data, params)
        Res <- list(rawResults = res)
        results <- list(Summary = Res)
    }
    else {
        res <- rma.peto(ai = binary.data@g1O1, bi = binary.data@g1O2, 
            ci = binary.data@g2O1, di = binary.data@g2O2, slab = binary.data@study.names, 
            level = params$conf.level, digits = params$digits)
        degf <- res$k.yi - 1
        model.title <- paste("Fixed-Effects Model - Peto (k = ", 
            res$k, ")", sep = "")
        summary.disp <- create.summary.disp(res, params, degf, 
            model.title)
        summary.disp
        if ((is.null(params$create.plot)) || (params$create.plot == 
            TRUE)) {
            forest.path <- paste(params$fp_outpath, sep = "")
            plot.data <- create.plot.data.binary(binary.data, 
                params, res)
            forest.plot(plot.data, outpath = forest.path)
            images <- c(`Forest Plot` = forest.path)
            plot.names <- c(`forest plot` = "forest_plot")
            results <- list(images = images, Summary = summary.disp, 
                plot_names = plot.names)
        }
        else {
            results <- list(Summary = summary.disp)
        }
    }
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
