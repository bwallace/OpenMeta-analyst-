\name{loo.ma.binary}
\alias{loo.ma.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loo.ma.binary(fname, binary.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
%%     ~~Describe \code{fname} here~~
}
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
function (fname, binary.data, params) 
{
    if (!("BinaryData" \%in\% class(binary.data))) 
        stop("Binary data expected.")
    loo.results <- array(dim = c(length(binary.data@study.names), 
        3))
    params$create.plot <- FALSE
    N <- length(binary.data@study.names)
    for (i in 1:N) {
        index.ls <- 1:N
        if (i == 1) {
            index.ls <- index.ls[2:N]
        }
        else if (i == N) {
            index.ls <- index.ls[1:N - 1]
        }
        else {
            index.ls <- c(index.ls[1:i - 1], index.ls[(i + 1):N])
        }
        y.tmp <- binary.data@y[index.ls]
        SE.tmp <- binary.data@SE[index.ls]
        names.tmp <- binary.data@study.names[index.ls]
        bin.data.tmp <- NULL
        if (length(binary.data@g1O1) > 0) {
            g1O1.tmp <- binary.data@g1O1[index.ls]
            g1O2.tmp <- binary.data@g1O2[index.ls]
            g2O1.tmp <- binary.data@g2O1[index.ls]
            g2O2.tmp <- binary.data@g2O2[index.ls]
            bin.data.tmp <- new("BinaryData", g1O1 = g1O1.tmp, 
                g1O2 = g1O2.tmp, g2O1 = g2O1.tmp, g2O2 = g2O2.tmp, 
                y = y.tmp, SE = SE.tmp, study.names = names.tmp)
        }
        else {
            bin.data.tmp <- new("BinaryData", y = y.tmp, SE = SE.tmp, 
                study.names = names.tmp)
        }
        cur.res <- eval(call(fname, bin.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep = ""), 
            cur.res))
        loo.results[i, ] <- cur.overall
    }
    study.names <- NULL
    for (count in 1:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("- ", binary.data@study.names[count], 
            sep = ""))
    }
    loo.disp <- create.overall.display(loo.results, study.names, 
        params)
    loo.disp
    forest.path <- "./r_tmp/loo_forest.png"
    addRow1Space <- FALSE
    plotData <- create.plot.data.overall(params, loo.results, 
        study.names, addRow1Space)
    forest.plot(plotData, outpath = forest.path)
    images <- c(`Leave-one-out Forest plot` = forest.path)
    plot.names <- c(`loo forest plot` = "loo_forest_plot")
    results <- list(images = images, `Leave-one-out Summary` = loo.disp, 
        plot_names = plot.names)
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
