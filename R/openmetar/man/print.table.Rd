\name{print.table}
\alias{print.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.table(table.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table.data}{
%%     ~~Describe \code{table.data} here~~
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
function (table.data) 
{
    num.rows <- length(table.data[, 1])
    num.cols <- length(table.data[1, ])
    col.widths <- NULL
    for (col.index in 1:num.cols) {
        col.widths <- c(col.widths, max(nchar(table.data[, col.index])) + 
            4)
    }
    table.width <- sum(col.widths) + num.cols + 1
    dash.line <- NULL
    for (count in 1:(table.width - 2)) {
        dash.line <- paste(dash.line, "-", sep = "")
    }
    top.line <- paste("+", dash.line, "+", sep = "")
    middleLine <- paste("|", dash.line, "|", sep = "")
    cat(top.line)
    cat("\n")
    for (row.index in 1:num.rows) {
        table.row <- "|"
        for (col.index in 1:num.cols) {
            col.width <- col.widths[col.index]
            entry <- pad.entry(table.data[row.index, col.index], 
                col.width)
            table.row <- paste(table.row, entry, "|", sep = "")
        }
        cat(table.row)
        cat("\n")
        if (row.index < num.rows) {
            cat(middleLine)
            cat("\n")
        }
    }
    cat(top.line)
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
