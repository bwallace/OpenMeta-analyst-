\name{meta.regression}
\alias{meta.regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meta.regression(reg.data, params, cov.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reg.data}{
%%     ~~Describe \code{reg.data} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{cov.name}{
%%     ~~Describe \code{cov.name} here~~
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
function (reg.data, params, cov.name) 
{
    cov.val.str <- paste("reg.data@covariates$", cov.name, sep = "")
    cov.vals <- eval(parse(text = cov.val.str))
    res <- rma.uni(yi = reg.data@y, sei = reg.data@SE, slab = reg.data@study.names, 
        level = params$conf.level, digits = params$digits, method = "DL", 
        mods = cov.vals)
    reg.disp <- create.regression.disp(res, params)
    reg.disp
    betas <- res$b
    fitted.line <- list(intercept = betas[1], slope = betas[2])
    plot.data <- create.plot.data.reg(reg.data, params, fitted.line, 
        selected.cov = cov.name)
    plot.path <- "./r_tmp/reg.png"
    meta.regression.plot(plot.data, plot.path, symSize = 1, lcol = "darkred", 
        metric = "Effect size", xlabel = plot.data$covariate$varname, 
        lweight = 3, lpatern = "dotted", plotregion = "n", mcolor = "darkgreen", 
        regline = TRUE)
    images <- c(`Regression Plot` = plot.path)
    plot.names <- c(`forest plot` = "reg.plot")
    results <- list(images = images, `Regression Summary` = reg.disp, 
        plot_names = plot.names)
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
