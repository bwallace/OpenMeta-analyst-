\name{fillin.cont.1spell}
\alias{fillin.cont.1spell}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillin.cont.1spell(n = NA, mean = NA, sd = NA, se = NA, var = NA, low = NA, high = NA, pval = NA, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{mean}{
%%     ~~Describe \code{mean} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{se}{
%%     ~~Describe \code{se} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{low}{
%%     ~~Describe \code{low} here~~
}
  \item{high}{
%%     ~~Describe \code{high} here~~
}
  \item{pval}{
%%     ~~Describe \code{pval} here~~
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
function (n = NA, mean = NA, sd = NA, se = NA, var = NA, low = NA, 
    high = NA, pval = NA, alpha = 0.05) 
{
    succeeded <- FALSE
    comment <- ""
    res <- list(succeeded = succeeded)
    z <- abs(qnorm(alpha/2))
    input.vector <- c(n, mean, sd, se, var, low, high, pval)
    input.pattern <- !(is.na(input.vector))
    if (is.na(mean)) {
        mean = try((high + low)/2, silent = TRUE)
    }
    if (is.na(mean)) {
        comment <- paste(comment, "no info on mean", sep = "|")
        return(c(res, comment = comment))
    }
    if (is.na(se)) {
        se = try(sqrt(var), silent = TRUE)
    }
    if (is.na(se)) {
        se = try(sqrt(sd^2)/(n - 1), silent = TRUE)
    }
    if (is.na(se)) {
        se = try(abs(high - low)/(2 * z), silent = TRUE)
    }
    if (is.na(se)) {
        se = try((mean - low)/z, silent = TRUE)
    }
    if (is.na(se)) {
        se = try((high - mean)/z, silent = TRUE)
    }
    if (is.na(se)) {
        se = try(-mean/qnorm(pval/2), silent = TRUE)
    }
    if (is.na(se)) {
        comment <- paste(comment, "no info on dispersion", sep = "|")
        return(c(res, comment = comment))
    }
    if (is.na(var)) {
        var = se^2
    }
    if (is.na(low)) {
        low = mean - z * se
    }
    if (is.na(high)) {
        high = mean + z * se
    }
    if (is.na(pval)) {
        pval = 2 * pnorm(-abs(mean/se))
    }
    if (is.na(sd)) {
        sd = try(var * (n - 1), silent = TRUE)
    }
    if (is.na(sd)) {
        comment <- paste(comment, "{n & sd} missing")
    }
    if (is.na(n)) {
        n = try(round(sd/var + 1), silent = TRUE)
    }
    succeeded <- check.1spell.res(n = n, se = se)$succeeded
    output.vector <- c(n, mean, sd, se, var, low, high, pval)
    output.names <- c("n", "mean", "sd", "se", "var", "low", 
        "high", "pval")
    names(output.vector) <- output.names
    res <- list(succeeded = succeeded, input.pattern = input.pattern, 
        output = output.vector, comment = comment)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
