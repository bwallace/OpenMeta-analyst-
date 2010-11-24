\name{fillin.2x2.simple}
\alias{fillin.2x2.simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example function for data.transforms}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillin.2x2.simple(c11 = NA, c12 = NA, c21 = NA, c22 = NA, r1sum = NA, r2sum = NA, c1sum = NA, c2sum = NA, total = NA, touse = rep(TRUE, 9))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c11}{
%%     ~~Describe \code{c11} here~~
}
  \item{c12}{
%%     ~~Describe \code{c12} here~~
}
  \item{c21}{
%%     ~~Describe \code{c21} here~~
}
  \item{c22}{
%%     ~~Describe \code{c22} here~~
}
  \item{r1sum}{
%%     ~~Describe \code{r1sum} here~~
}
  \item{r2sum}{
%%     ~~Describe \code{r2sum} here~~
}
  \item{c1sum}{
%%     ~~Describe \code{c1sum} here~~
}
  \item{c2sum}{
%%     ~~Describe \code{c2sum} here~~
}
  \item{total}{
%%     ~~Describe \code{total} here~~
}
  \item{touse}{
%%     ~~Describe \code{touse} here~~
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
function (c11 = NA, c12 = NA, c21 = NA, c22 = NA, r1sum = NA, 
    r2sum = NA, c1sum = NA, c2sum = NA, total = NA, touse = rep(TRUE, 
        9)) 
{
    y <- c(rep(c11, 4), rep(c12, 4), rep(c21, 4), res <- rep(c22, 
        4), rep(r1sum, 3), rep(r2sum, 3), rep(c1sum, 3), rep(c2sum, 
        3), rep(total, 8))
    select <- c(rep(touse[1], 4), rep(touse[2], 4), rep(touse[3], 
        4), rep(touse[4], 4), rep(touse[5], 3), rep(touse[6], 
        3), rep(touse[7], 3), rep(touse[8], 3), rep(touse[9], 
        8))
    X <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 
        0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, -1, -1, -1, 0, 0, 0, 
        0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 
        0, 0, 0, 0, 0, -1, 0, 0, 0, 1, 0, -1, 0, -1, -1, 0, 0, 
        0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 
        0, 0, 0, -1, 0, 0, 0, 0, 0, 1, 0, 0, -1, -1, 0, -1, 0, 
        0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 
        1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 1, 0, -1, -1, -1, 0, 
        0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 
        0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        -1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, 
        0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 
        0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 
        0, 0, 0, 1, 0)
    X <- matrix(X, ncol = 9, byrow = TRUE)
    my.frame <- as.data.frame(X)
    colnames(my.frame) <- c("c11", "c12", "c21", "c22", "r1sum", 
        "r2sum", "c1sum", "c2sum", "total")
    my.frame <- cbind(y, my.frame)
    res <- lm(y ~ c11 + c12 + c21 + c22 + r1sum + r2sum + c1sum + 
        c2sum + total + (-1), data = my.frame)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
