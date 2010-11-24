\name{study.column}
\alias{study.column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Study Column Data for Forest Plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
study.column(forest.data, title.font = "bold")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest.data}{
%%     ~~Describe \code{forest.data} here~~
}
  \item{title.font}{
%%     ~~Describe \code{title.font} here~~
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
function (forest.data, title.font = "bold") 
{
    content <- rep(NA, length(forest.data$label))
    for (i in 1:length(forest.data$label)) {
        if (forest.data$types[i] != 0) 
            content[i] <- list(textGrob(forest.data$label[i], 
                x = 0, just = "left", gp = gpar(fontface = title.font)))
        else content[i] <- list(textGrob(forest.data$label[i], 
            x = 0, just = "left", gp = gpar(fontface = "plain")))
    }
    rows <- c(1, rep(NA, (length(forest.data$label) - 1)))
    for (i in 1:(length(forest.data$label) - 1)) {
        if (forest.data$types[i] == 3 && forest.data$types[i + 
            1] == 0) 
            rows[i + 1] <- rows[i] + 2
        else if (forest.data$types[i] == 0 && forest.data$types[i + 
            1] == 2) 
            rows[i + 1] <- rows[i] + 1
        else if (forest.data$types[i] == 0 && forest.data$types[i + 
            1] == 1) 
            rows[i + 1] <- rows[i] + 1
        else if (forest.data$types[i] == 1 && forest.data$types[i + 
            1] == 0) 
            rows[i + 1] <- rows[i] + 2
        else if (forest.data$types[i] == 1 && forest.data$types[i + 
            1] == 2) 
            rows[i + 1] <- rows[i] + 2
        else rows[i + 1] <- rows[i] + 1
    }
    study.column.list <- list(content = content, rows = rows)
    study.column.list
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
