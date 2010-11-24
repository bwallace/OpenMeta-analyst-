\name{create.summary.disp}
\alias{create.summary.disp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create summary of meta-analysis results}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create.summary.disp(res, params, degf, model.title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{params}{
%%     ~~Describe \code{params} here~~
}
  \item{degf}{
%%     ~~Describe \code{degf} here~~
}
  \item{model.title}{
%%     ~~Describe \code{model.title} here~~
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
function (res, params, degf, model.title) 
{
    QLabel = paste("Q(df = ", degf, ")", sep = "")
    I2 <- max(0, (res$QE - degf)/res$QE)
    I2 <- paste(100 * round(I2, digits = 2), "\%")
    QE <- round(res$QE, digits = params$digits)
    QEp <- round.display(res$QEp, digits = params$digits)
    het.array <- array(c(QLabel, QE, "p-Value", QEp, "I^2", I2), 
        dim = c(2, 3))
    class(het.array) <- "table"
    het.data <- list(Title = "  Test for Heterogeneity", het.table = het.array)
    est.disp <- round(binary.transform.f(params$measure)$display.scale(res$b), 
        digits = params$digits)
    lb.disp <- round(binary.transform.f(params$measure)$display.scale(res$ci.lb), 
        digits = params$digits)
    ub.disp <- round(binary.transform.f(params$measure)$display.scale(res$ci.ub), 
        digits = params$digits)
    pVal <- round.display(res$pval, digits = params$digits)
    zVal <- round(res$zval, digits = params$digits)
    se <- round(res$se, digits = params$digits)
    if (binary.transform.f(params$measure)$display.scale(1) != 
        binary.transform.f(params$measure)$calc.scale(1)) {
        res.array <- array(c("Estimate", est.disp, "p-Value", 
            pVal, "Z-Value", zVal, "Lower bound", lb.disp, "Upper bound", 
            ub.disp), dim = c(2, 5))
        estCalc <- round(res$b, digits = params$digits)
        lbCalc <- round(res$ci.lb, digits = params$digits)
        ubCalc <- round(res$ci.ub, digits = params$digits)
        alt.array <- array(c("Estimate", estCalc, "SE", se, "Lower bound", 
            lbCalc, "Upper bound", ubCalc), dim = c(2, 4))
        class(alt.array) <- "table"
        alt.data <- list(Title = "  Model Results (calculation scale)", 
            alt.table = alt.array)
    }
    else {
        res.array <- array(c("Estimate", est.disp, "SE", se, 
            "p-Value", pVal, "Z-Value", zVal, "Lower bound", 
            lb.disp, "Upper bound", ub.disp), dim = c(2, 6))
        alt.data <- list(Title = NA)
    }
    class(res.array) <- "table"
    result.data <- list(Title = "  Model Results (reporting scale)", 
        result.table = res.array)
    summary.disp <- list(modelTitle = model.title, het.data = het.data, 
        result.data = result.data, alt.data = alt.data, rawResults = res)
    class(summary.disp) <- "summary.display"
    return(summary.disp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
