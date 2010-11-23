\name{loo.ma.continuous}
\alias{loo.ma.continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loo.ma.continuous(fname, cont.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
%%     ~~Describe \code{fname} here~~
}
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
function (fname, cont.data, params) 
{
    if (!("ContinuousData" \%in\% class(cont.data))) 
        stop("Continuous data expected.")
    loo.results <- array(dim = c(length(cont.data@study.names), 
        3))
    params$create.plot <- FALSE
    N <- length(cont.data@study.names)
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
        y.tmp <- cont.data@y[index.ls]
        SE.tmp <- cont.data@SE[index.ls]
        names.tmp <- cont.data@study.names[index.ls]
        cont.data.tmp <- NULL
        if (length(cont.data@N1) > 0) {
            N1.tmp <- cont.data@N1[index.ls]
            mean1.tmp <- cont.data@mean1[index.ls]
            sd1.tmp <- cont.data@sd1[index.ls]
            N2.tmp <- cont.data@N2[index.ls]
            mean2.tmp <- cont.data@mean2[index.ls]
            sd2.tmp <- cont.data@sd2[index.ls]
            cont.data.tmp <- new("ContinuousData", N1 = N1.tmp, 
                mean1 = mean1.tmp, sd1 = sd1.tmp, N2 = N2.tmp, 
                mean2 = mean2.tmp, sd2 = sd2.tmp, y = y.tmp, 
                SE = SE.tmp, study.names = names.tmp)
        }
        else {
            cont.data.tmp <- new("ContinuousData", y = y.tmp, 
                SE = SE.tmp, study.names = names.tmp)
        }
        cur.res <- eval(call(fname, cont.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep = ""), 
            cur.res))
        loo.results[i, ] <- cur.overall
    }
    study.names <- NULL
    for (count in 1:length(cont.data@study.names)) {
        study.names <- c(study.names, paste("- ", cont.data@study.names[count], 
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
