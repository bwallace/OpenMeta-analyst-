\name{fillin.cont.AminusB}
\alias{fillin.cont.AminusB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility function for data_transform}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillin.cont.AminusB(n.A = NA, mean.A = NA, sd.A = NA, se.A = NA, var.A = NA, low.A = NA, high.A = NA, pval.A = NA, n.B = NA, mean.B = NA, sd.B = NA, se.B = NA, var.B = NA, low.B = NA, high.B = NA, pval.B = NA, correlation = 0, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.A}{
%%     ~~Describe \code{n.A} here~~
}
  \item{mean.A}{
%%     ~~Describe \code{mean.A} here~~
}
  \item{sd.A}{
%%     ~~Describe \code{sd.A} here~~
}
  \item{se.A}{
%%     ~~Describe \code{se.A} here~~
}
  \item{var.A}{
%%     ~~Describe \code{var.A} here~~
}
  \item{low.A}{
%%     ~~Describe \code{low.A} here~~
}
  \item{high.A}{
%%     ~~Describe \code{high.A} here~~
}
  \item{pval.A}{
%%     ~~Describe \code{pval.A} here~~
}
  \item{n.B}{
%%     ~~Describe \code{n.B} here~~
}
  \item{mean.B}{
%%     ~~Describe \code{mean.B} here~~
}
  \item{sd.B}{
%%     ~~Describe \code{sd.B} here~~
}
  \item{se.B}{
%%     ~~Describe \code{se.B} here~~
}
  \item{var.B}{
%%     ~~Describe \code{var.B} here~~
}
  \item{low.B}{
%%     ~~Describe \code{low.B} here~~
}
  \item{high.B}{
%%     ~~Describe \code{high.B} here~~
}
  \item{pval.B}{
%%     ~~Describe \code{pval.B} here~~
}
  \item{correlation}{
%%     ~~Describe \code{correlation} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (n.A = NA, mean.A = NA, sd.A = NA, se.A = NA, var.A = NA, 
    low.A = NA, high.A = NA, pval.A = NA, n.B = NA, mean.B = NA, 
    sd.B = NA, se.B = NA, var.B = NA, low.B = NA, high.B = NA, 
    pval.B = NA, correlation = 0, alpha = 0.05) 
{
    succeeded <- TRUE
    comment <- ""
    res <- list(succeeded = succeeded)
    n.diff <- NA
    mean.diff <- NA
    sd.diff <- NA
    se.diff <- NA
    var.diff <- NA
    low.diff <- NA
    high.diff <- NA
    pval.diff <- NA
    z <- abs(qnorm(alpha/2))
    input.vector.A <- c(n.A, mean.A, sd.A, se.A, var.A, low.A, 
        high.A, pval.A)
    input.vector.B <- c(n.B, mean.B, sd.B, se.B, var.B, low.B, 
        high.B, pval.B)
    input.pattern <- list(A = !(is.na(input.vector.A)), B = !(is.na(input.vector.B)))
    fillin.A <- fillin.cont.1spell(n.A, mean.A, sd.A, se.A, var.A, 
        low.A, high.A, pval.A, alpha = alpha)
    comment <- paste(comment, paste("A", fillin.A$comment, sep = ":"), 
        sep = "|")
    fillin.B <- fillin.cont.1spell(n.B, mean.B, sd.B, se.B, var.B, 
        low.B, high.B, pval.B, alpha = alpha)
    comment <- paste(comment, paste("B", fillin.B$comment, sep = ":"), 
        sep = "|")
    if (identical(c(fillin.A$succeeded, fillin.B$succeeded), 
        c(TRUE, TRUE))) {
        mean.diff <- fillin.A$output["mean"] - fillin.B$output["mean"]
        var.diff <- (fillin.A$output["se"])^2 + (fillin.B$output["se"])^2
        -2 * correlation * (fillin.A$output["se"]) * (fillin.B$output["se"])
        se.diff <- sqrt(var.diff)
        low.diff <- mean.diff - z * se.diff
        high.diff <- mean.diff + z * se.diff
        pval.diff <- 2 * pnorm(-abs(mean.diff/se.diff))
        n.diff <- try(min(fillin.A$output["n"], fillin.B$output["n"]), 
            silent = TRUE)
        sd.diff <- try(var.diff * (n.diff - 1), silent = TRUE)
    }
    else {
        succeeded <- FALSE
    }
    output.vector <- c(n.diff, mean.diff, sd.diff, se.diff, var.diff, 
        low.diff, high.diff, pval.diff)
    output.names <- c("n", "mean", "sd", "se", "var", "low", 
        "high", "pval")
    names(output.vector) <- output.names
    res <- list(succeeded = succeeded, input.pattern = input.pattern, 
        output = output.vector, pre = fillin.A$output, post = fillin.B$output, 
        comment = comment, correlation = correlation)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
