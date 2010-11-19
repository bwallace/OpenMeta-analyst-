### Name: binary.fixed.mh

### Aliases: binary.fixed.mh
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (binaryData, params) 
{
    if (!("BinaryData" %in% class(binaryData))) 
        stop("Binary data expected.")
    results <- NULL
    if (length(binaryData@g1O1) == 1 || length(binaryData@y) == 
        1) {
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list(summary = res)
    }
    else {
        res <- rma.mh(ai = binaryData@g1O1, bi = binaryData@g1O2, 
            ci = binaryData@g2O1, di = binaryData@g2O2, slab = binaryData@studyNames, 
            level = params$conf.level, digits = params$digits)
        degf <- res$k.yi - 1
        summaryDisp <- createSummaryDisp(res, params, degf)
        summaryDisp$modelTitle <- paste("Fixed-Effects Model - Mantel Haenszel (k = ", 
            res$k, ")", sep = "")
        summaryDisp
        forest_path <- "./r_tmp/forest.png"
        plotData <- create.plot.data.binary(binaryData, params, 
            res)
        forest.plot(plotData, outpath = forest_path)
        images <- c(`forest plot` = forest_path)
        plot_names <- c(`forest plot` = "forest_plot")
        results <- list(images = images, summary = summary, plot_names = plot_names)
    }
    results
  }



