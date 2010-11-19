### Name: continuous.random

### Aliases: continuous.random
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (contData, params) 
{
    if (!("ContinuousData" %in% class(contData))) 
        stop("Continuous data expected.")
    results <- NULL
    if (length(contData@studyNames) == 1) {
        res <- get.res.for.one.cont.study(contData, params)
        results <- list(summary = res)
    }
    else {
        if (length(contData@mean1) > 0) {
            res <- rma.uni(n1i = contData@N1, n2i = contData@N2, 
                m1i = contData@mean1, m2i = contData@mean2, sd1i = contData@sd1, 
                sd2i = contData@sd2, slab = contData@studyNames, 
                method = params$rm.method, measure = params$measure, 
                level = params$conf.level, digits = params$digits)
        }
        else {
            res <- rma.uni(yi = contData@y, sei = contData@SE, 
                slab = contData@studyNames, method = params$rm.method, 
                level = params$conf.level, digits = params$digits)
        }
        getwd()
        forest.path <- "./r_tmp/forest.png"
        plotData <- create.plot.data.continuous(contData, params, 
            res)
        forest.plot(plotData, outpath = forest.path)
        images <- c(`forest plot` = forest.path)
        plot_names <- c(`forest plot` = "forest_plot")
        results <- list(images = images, summary = res, plot_names = plot_names)
    }
    results
  }



