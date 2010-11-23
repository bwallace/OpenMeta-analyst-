\name{continuous.random}
\alias{continuous.random}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
continuous.random(cont.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cont.data}{
%%     ~~Describe \code{cont.data} here~~
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
function (cont.data, params) 
{
    if (!("ContinuousData" \%in\% class(cont.data))) 
        stop("Continuous data expected.")
    results <- NULL
    if (length(cont.data@study.names) == 1) {
        res <- get.res.for.one.cont.study(cont.data, params)
        Res <- list(rawResults = res)
        results <- list(Summary = Res)
    }
    else {
        if (length(cont.data@mean1) > 0) {
            res <- rma.uni(n1i = cont.data@N1, n2i = cont.data@N2, 
                m1i = cont.data@mean1, m2i = cont.data@mean2, 
                sd1i = cont.data@sd1, sd2i = cont.data@sd2, slab = cont.data@study.names, 
                method = params$rm.method, measure = params$measure, 
                level = params$conf.level, digits = params$digits)
        }
        else {
            res <- rma.uni(yi = cont.data@y, sei = cont.data@SE, 
                slab = cont.data@study.names, method = params$rm.method, 
                level = params$conf.level, digits = params$digits)
        }
        degf <- res$k - res$p
        model.title <- paste("Continuous Random-Effects Model (k = ", 
            res$k, ")", sep = "")
        summary.disp <- create.summary.disp(res, params, degf, 
            model.title)
        summary.disp
        if ((is.null(params$createPlot)) || (params$createPlot == 
            TRUE)) {
            forest.path <- paste(params$fp_outpath, sep = "")
            plot.data <- create.plot.data.continuous(cont.data, 
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
