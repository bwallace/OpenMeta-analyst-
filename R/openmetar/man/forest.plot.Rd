\name{forest.plot}
\alias{forest.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create forest plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
forest.plot(forest.data, outpath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest.data}{
%%     ~~Describe \code{forest.data} here~~
}
  \item{outpath}{
%%     ~~Describe \code{outpath} here~~
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
function (forest.data, outpath) 
{
    study.col <- study.column(forest.data, "bold")
    additional.cols <- c()
    if (length(forest.data$additional.col.data) > 0) {
        additional.cols <- additional.columns(forest.data, "bold")
    }
    effects.col <- effectsize.column(forest.data)
    forest.plot.params <- plot.options(forest.data, box.sca = 0.8, 
        gapSize = 3.2, plotWidth = 5)
    extra.space <- sum(forest.data$types != 0)
    height <- length(forest.data$types) + extra.space
    if (length(forest.data$additional.col.data) > 0) {
        width.list <- vector("list")
        width.list[[1]] <- unit.c(max(unit(rep(1, length(forest.data$label)), 
            "grobwidth", additional.cols[[1]]$content)), forest.plot.params$col.gap)
        if (length(forest.data$additional.col.data) > 1) {
            for (i in 2:length(additional.cols)) {
                width.list[[i]] <- unit.c(width.list[[i - 1]], 
                  max(unit(rep(1, length(forest.data$label)), 
                    "grobwidth", additional.cols[[i]]$content)), 
                  forest.plot.params$col.gap)
            }
        }
        how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
            "grobwidth", study.col$content)), "inches", valueOnly = TRUE) + 
            convertX(forest.plot.params$col.gap, "inches", valueOnly = TRUE) + 
            sum(convertX(unit.c(width.list[[length(additional.cols)]]), 
                "inches", valueOnly = TRUE)) + length(additional.cols) * 
            convertX(forest.plot.params$col.gap, "inches", valueOnly = TRUE) + 
            convertX(forest.plot.params$effect.col.width, "inches", 
                valueOnly = TRUE)
        how.tall <- convertY(unit(rep(1, height), "lines"), "inches", 
            valueOnly = TRUE)
        png(file = outpath, width = how.wide + 1, height = height * 
            how.tall + 2, units = "in", res = 144)
        pushViewport(viewport(layout = grid.layout(height, 2 * 
            length(additional.cols) + 3, width = unit.c(max(unit(rep(1, 
            length(forest.data$label)), "grobwidth", study.col$content)), 
            forest.plot.params$col.gap, width.list[[length(additional.cols)]], 
            forest.plot.params$effect.col.width), height = unit(rep(1, 
            height), "lines"))))
    }
    else {
        how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
            "grobwidth", study.col$content)), "inches", valueOnly = TRUE) + 
            convertX(forest.plot.params$col.gap, "inches", valueOnly = TRUE) + 
            convertX(forest.plot.params$effect.col.width, "inches", 
                valueOnly = TRUE)
        how.tall <- convertY(unit(rep(1, height), "lines"), "inches", 
            valueOnly = TRUE)
        png(file = outpath, width = how.wide + 1, height = height * 
            how.tall + 2, units = "in", res = 144)
        pushViewport(viewport(layout = grid.layout(height, 2 * 
            length(additional.cols) + 3, width = unit.c(max(unit(rep(1, 
            length(forest.data$label)), "grobwidth", study.col$content)), 
            forest.plot.params$col.gap, forest.plot.params$effect.col.width), 
            height = unit(rep(1, height), "lines"))))
    }
    draw.label.col(study.col, 1)
    if (length(additional.cols) > 0) {
        for (i in 1:length(additional.cols)) {
            draw.label.col(additional.cols[[i]], 1 + 2 * i)
        }
    }
    effects.col$range <- forest.plot.params$effect.col.range
    effects.col$sizes <- forest.plot.params$effect.col.sizes
    effects.col$width <- forest.plot.params$effect.col.width
    xticks <- forest.data$fp_xticks
    if (!is.null(xticks)) {
        if (xticks == "NULL") {
            xticks <- NULL
        }
        else {
            xticks <- eval(parse(text = paste("c(", xticks, ")", 
                sep = "")))
        }
    }
    effect.size.str <- c(paste(forest.data$fp_xlabel, sep = ""))
    draw.data.col(forest.data, effects.col, 2 * length(additional.cols) + 
        3, color.overall = "lightblue", color.subgroup = "yellow", 
        summary.line.col = "red", summary.line.pat = "dashed", 
        metric = effect.size.str, diam.size = 1.2, user.ticks = xticks)
    graphics.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
