####################################
# OpenMeta[Analyst]                #
# ----                             #
# binary_methods.r                 # 
# Facade module; wraps methods     #
# that perform analysis on binary  #
# data in a coherent interface.    # 
####################################


#bd <- new('BinaryData', g1O1=c(30, 20, 10), g1O2=c(270, 180, 90), g2O1=c(35, 25, 15), g2O2=c(265, 175, 85), y=c(0.62962962963, 0.777777777778, 0.84126984127), SE=c(0.432831413165, 0.343592092809, 0.290047070662), studyNames=c('lau', 'wallace', 'trik'), covariates=list(hi=c(1,2,3)))


# bd <- new("BinaryData", g1O1=c(10, 20, 30), g1O2=c(90, 180, 270), g2O1=c(15, 25, 35), g2O2=c(85, 175, 265),                       studyNames=c("1", "2", "3")
# params <- list(metric="OR", conf.level=95, digits=3)

library(metafor)

binary.log.metrics <- c("OR", "RR", "PETO")

compute.for.one.bin.study <- function(binary.data, params){
    res <- escalc(params$metric, ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                    ci=binary.data@g2O1, di=binary.data@g2O2)
    res                             
}

binary.transform.f <- function(metric.str){
    display.scale <- function(x){
        if (metric.str %in% binary.log.metrics){
            exp(x)
        }
        else {
        	# identity function
            x
        }
    }
    
    calc.scale <- function(x){
        if (metric.str %in% binary.log.metrics){
            log(x)
        }
        else {
        	# identity function
            x
        }    
    }
    list(display.scale = display.scale, calc.scale = calc.scale)
}

get.res.for.one.binary.study <- function(binaryData, params){
    # this method can be called when there is only one study to 
    # get the point estimate and lower/upper bounds.
    y<-NULL
    se<-NULL
    if (is.na(binaryData@y)){
        res <- compute.for.one.bin.study(binaryData, params)    
        y <- res$yi[1]
        se <- sqrt(res$vi[1])
    }
    else{
        y <- binaryData@y[1]
        se <- binaryData@SE[1]
    }
    # note: conf.level is given as, e.g., 95, rather than .95.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    ub <- y + mult*se
    lb <- y - mult*se
    # we make lists to comply with the get.overall method
    res <- list("b"=c(y), "ci.lb"=lb, "ci.ub"=ub) 
    res
}

extractData <- function(binaryData, params){
    # Extracts data from binaryData into an array and computes bounds on confidence intervals.
    
    # Compute bounds on confidence intervals.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    LL <- round(exp(binaryData@y - mult*binaryData@SE), digits = params$digits)
    UL <- round(exp(binaryData@y + mult*binaryData@SE), digits = params$digits)
    # Extract the data from binaryData and round
    eventT <- round(binaryData@g1O1, digits = params$digits)
    subjectT <- round(binaryData@g1O1 + binaryData@g1O2, digits = params$digits)
    eventC <- round(binaryData@g2O1, digits = params$digits)
    subjectC <- round(binaryData@g2O1 + binaryData@g2O2, digits = params$digits)
    y <- round(binaryData@y, digits = params$digits) 
    rawData <- array(c(binaryData@studyNames, eventT, subjectT, eventC, subjectC, y, LL, UL), 
                  dim = c(length(binaryData@studyNames), 8))  
    class(rawData) <- c("extractData")
    return(rawData)
}

print.extractData <- function(rawData, ...){
        
    # Create data frame to display the data
    dframe <- data.frame(" " = c("Study", "", rawData[,1]), 
                    " " = c("Events (T)", "", rawData[,2]), 
                    " " = c("Subjects (T)", "", rawData[,3]),
                    " " = c("Events (C)", "", rawData[,4]), 
                    " " = c("Subjects (T)", "", rawData[,5]),
                    " " = c("Effect size", "", rawData[,6]),  
                    " " = c("Lower bound", "", rawData[,7]), 
                    " " = c("Upper bound", "", rawData[,8]), check.names = FALSE)
    displayResults <- format(dframe, justify = "centre", quote = FALSE, width = 12)
    displayResults[1] <- format(dframe[1], justify = "left", quote = False, width = 12)
    displayResults
    print(displayResults, row.names=FALSE)
}

# TODO this should be moved to plotting.r

create.plot.data <- function(binaryData, params, res, selected.cov = NULL, include.overall=TRUE){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    plotData <- list( label = c("Studies", binaryData@studyNames, "Overall"),
                types = c(3, rep(0, length(binaryData@studyNames)), 2),
                scale = "log" )

    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    lb <- binaryData@y - mult*binaryData@SE
    ub <- binaryData@y + mult*binaryData@SE


    ### TODO only do this for appropriate metrics
    # i.e., ratios, which will be on the log-scale
    # exponentiate effect sizes and bounds.
    y <- exp(binaryData@y)
    lb <- exp(lb)
    ub <- exp(ub)

    yOverall <- exp(res$b[1])
    lbOverall <- exp(res$ci.lb[1])
    ubOverall <- exp(res$ci.ub[1])

    # round results for display.
    yRounded <- round(y, digits = params$digits)
    lbRounded <- round(lb, digits = params$digits)
    ubRounded <- round(ub, digits = params$digits)
    yOverallRounded <- round(yOverall, digits = params$digits)
    lbOverallRounded <- round(lbOverall, digits = params$digits)
    ubOverallRounded <- round(ubOverall, digits = params$digits)

    additional.cols <- list(es = c("ES (LL, UL)", paste(yRounded, " (", lbRounded, " , ", ubRounded, ")", sep = ""),
                                 paste(yOverallRounded, " (", lbOverallRounded, " , ", ubOverallRounded, ")", sep = "")))
        
    # if we have raw data, add it to the additional columns
    if (length(binaryData@g1O1) > 0) {
        additional.cols$cases = c("Ev/Trt", 
                                    paste(binaryData@g1O1, " / ", binaryData@g1O1 + binaryData@g1O2, sep = ""), 
                                    paste(sum(binaryData@g1O1), " / ", sum(binaryData@g1O1 + binaryData@g1O2), sep = ""))
        additional.cols$controls = c("Ev/Ctrl", 
                                        paste(binaryData@g1O1, " / ", binaryData@g1O1 + binaryData@g1O2, sep = ""),
                                        paste(sum(binaryData@g1O1), " / ", sum(binaryData@g1O1 + binaryData@g1O2), sep = ""))
    }

    plotData$additional.col.data <- additional.cols
    if (!is.null(selected.cov)){
        cov.val.str <- paste("binaryData@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plotData$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }
    
    effects <- list(ES = c(y, yOverall),
                    LL = c(lb, lbOverall),
                    UL = c(ub, ubOverall))
    plotData$effects <- effects
    plotData
}

print.summaryDisplay <- function(x,...) {
    # Round stats
    QEDisp <- roundedNumDisplay(round(x$QE, x$digits), x$digits)
    QEpDisp <- roundedNumDisplay(round(x$QEp, x$digits), x$digits)
    pDisp <- roundedNumDisplay(round(x$PVal, x$digits), x$digits)
    hTest <- data.frame(" " = c("|"),
                        " " = paste("Q(df = ", x$df, ") = ", 
                                        QEDisp, sep = ""),
                        " " = c("|"),
                        " " = paste("p-Value ", QEpDisp, sep = ""), 
                        " " = c("|"), check.names = FALSE)
    hTestDisp<-format(hTest, justify = "centre", width=15)
    hTestDisp[1]<-format(hTest[1], justify="left", width = 2)
    hTestDisp[3]<-format(hTest[3], justify="centre", width = 2)
    hTestDisp[5]<-format(hTest[5], justify="right", width = 2) 
    hLine1 <- " +----------------------------------------+"
    
    cat(x$modelDesc)
    cat("\n\n")
    cat(x$hetTest)
    cat("\n")
    cat(hLine1)
    print(hTestDisp, row.names=FALSE)
    cat(hLine1)
    cat("\n\n")
    hLine2 <- " +----------------------------------------------------------------------------------+"
    hLine2middle <- " |----------------------------------------------------------------------------------|"
    if (!is.na(x$modelLogLabel)) {
        cat(x$modelLogLabel)
        cat("\n")
        
        modelLogLabels <- data.frame(" " = c("|"),
                            " " = c("Estimate"), 
                            " " = c("|"),
                            " " = c("SE"),
                            " " = c("|"), 
                            " " = c("z-Value"),
                            " " = c("|"), 
                            " " = c("p-Value"), 
                            " " = c("|"),
                            " " = c("Lower bound"), 
                            " " = c("|"),
                            " " = c("Upper bound"), 
                            " " = c("|"), check.names = FALSE) 
        modelLogRes <- data.frame(" " = c("|"),
                            " " = c(as.character(round(exp(x$estimate), digits=x$digits))), 
                            " " = c("|"),
                            " " = c(as.character(round(x$SE, digits=digits))),
                            " " = c("|"), 
                            " " = c(as.character(round(x$ZVal, digits=digits))),
                            " " = c("|"), 
                            " " = c(as.character(pDisp)), 
                            " " = c("|"),
                            " " = c(as.character(round(exp(x$LowerBound), digits=digits))), 
                            " " = c("|"),
                            " " = c(as.character(round(exp(x$UpperBound), digits=digits))), 
                            " " = c("|"), check.names = FALSE) 
        modelLogLabels[c(2,6,8)] <- format(modelLogLabels[c(2,6,8)], justify = "centre", width = 10)
        modelLogLabels[4] <- format(modelLogLabels[4], justify = "centre", width = 5)
        modelLogLabels[c(10,12)] <- format(modelLogLabels[c(10,12)], justify = "centre", width = 12)
        modelLogRes[c(2,6,8)] <- format(modelLogRes[c(2,6,8)], justify = "centre", width = 10)
        modelLogRes[4] <- format(modelLogRes[4], justify = "centre", width = 5)
        modelLogRes[c(10,12)] <- format(modelLogRes[c(10,12)], justify = "centre", width = 12)
        modelLogLabels[c(1,3,5,7,9,11,13)] <- format(modelLogLabels[c(1,3,5,7,9,11,13)], justify="centre", width=2)
        modelLogRes[c(1,3,5,7,9,11,13)] <- format(modelLogRes[c(1,3,5,7,9,11,13)], justify="centre", width=2)
        cat(hLine2)
        print(modelLogLabels, row.names=FALSE)
        cat(hLine2middle)
        print(modelLogRes, row.names=FALSE)
        cat(hLine2)
        cat("\n\n")
    }
    
    if (!is.na(x$modelStdLabel)) {
        cat(x$modelStdLabel)
        cat("\n")
        
        modelStdLabels <- data.frame(" " = c("|"),
                            " " = c("Estimate"), 
                            " " = c("|"),
                            " " = c("SE"),
                            " " = c("|"), 
                            " " = c("z-Value"),
                            " " = c("|"), 
                            " " = c("p-Value"), 
                            " " = c("|"),
                            " " = c("Lower bound"), 
                            " " = c("|"),
                            " " = c("Upper bound"), 
                            " " = c("|"), check.names = FALSE) 
        modelStdRes <- data.frame(" " = c("|"),
                            " " = c(as.character(round(exp(x$estimate), digits=x$digits))), 
                            " " = c("|"),
                            " " = c(as.character(round(x$SE, digits=digits))),
                            " " = c("|"), 
                            " " = c(as.character(round(x$ZVal, digits=digits))),
                            " " = c("|"), 
                            " " = c(as.character(pDisp)), 
                            " " = c("|"),
                            " " = c(as.character(round(exp(x$LowerBound), digits=digits))), 
                            " " = c("|"),
                            " " = c(as.character(round(exp(x$UpperBound), digits=digits))), 
                            " " = c("|"), check.names = FALSE) 
        modelStdLabels[c(2,6,8)] <- format(modelStdLabels[c(2,6,8)], justify = "centre", width = 10)
        modelStdLabels[4] <- format(modelStdLabels[4], justify = "centre", width = 5)
        modelStdLabels[c(10,12)] <- format(modelStdLabels[c(10,12)], justify = "centre", width = 12)
        modelStdRes[c(2,6,8)] <- format(modelStdRes[c(2,6,8)], justify = "centre", width = 10)
        modelStdRes[4] <- format(modelStdRes[4], justify = "centre", width = 5)
        modelStdRes[c(10,12)] <- format(modelStdRes[c(10,12)], justify = "centre", width = 12)
        modelStdLabels[c(1,3,5,7,9,11,13)] <- format(modelStdLabels[c(1,3,5,7,9,11,13)], justify="centre", width=2)
        modelStdRes[c(1,3,5,7,9,11,13)] <- format(modelStdRes[c(1,3,5,7,9,11,13)], justify="centre", width=2)
        cat(hLine2)
        print(modelStdLabels, row.names=FALSE)
        cat(hLine2middle)
        print(modelStdRes, row.names=FALSE)
        cat(hLine2)
        cat("\n")
    }
}

roundedNumDisplay <- function(x, digits) {
    # Prints "< 10^(-digits)" if x is < 10^(-digits)
    xDisp<-x
    if (x < 10^(-digits)) {
        xDisp <- paste("< ", 10^(-digits), sep = "", collapse = "")
    }
    return(xDisp)
}

###################################################
# binary fixed effects -- inverse variance        #
###################################################
binary.fixed.inv.var <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  
    
    results <- NULL
    if (length(binaryData@g1O1) == 1 || length(binaryData@y) == 1){
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list("summary"=res)
    }
    else{
        # call out to the metafor package
        res<-rma.uni(yi=binaryData@y, sei=binaryData@SE, slab=binaryData@studyNames,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
        
        forest_path <- "./r_tmp/forest.png"
        plotData <- create.plot.data.binary(binaryData, params, res)
        forest.plot(plotData, outpath=forest_path)
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("forest plot"=forest_path)
        plot_names <- c("forest plot"="forest_plot")
        
        
        ###
        # should we return the name of the result object & the name of the
        # plotting function as well here? perhaps only for the forest plot? 
        # this would allow interactive plot refinement via the console...
        modelDesc <- paste("Fixed-Effects Model - Inverse Variance (k = ", res$k, ")", sep="")
        hetTest <- "  Test for Heterogeneity:"       
        modelStdLabel <- "  Model Results (standard scale):"
        modelLogLabel <- "  Model Results (log scale):"
        summaryDisp <- list("digits"=params$digits, "modelDesc"=modelDesc, "hetTest"=hetTest, "modelStdLabel"=modelStdLabel, 
                            "modelLogLabel"=NA, "df"=res$k - res$p, "QE"=res$QE, "QEp"=res$QEp, "estimate"=res$b, 
                            "SE"=res$se, "ZVal"=res$zval, "PVal"=res$pval, "LowerBound"=res$ci.lb, "UpperBound"=res$ci.ub) 
        class(summaryDisp) <- "summaryDisplay"                    
        results <- list("images"=images, "summary"=summaryDisp, "plot_names"=plot_names)
    }
    results
}

                                
binary.fixed.inv.var.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    apply_adjustment_to = c("only0", "all")
    
    params <- list("measure"=binary_metrics, "conf.level"="float", "digits"="float", 
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("measure"="OR", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("measure", "conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.inv.var.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$summary
    overall <- list(c("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub))
    overall
}

############################################
#  binary fixed effects -- mantel haenszel #
############################################
binary.fixed.mh <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  

    results <- NULL
    if (length(binaryData@g1O1) == 1 || length(binaryData@y) == 1){
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list("summary"=res)
    }
    else{
        res<-rma.mh(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                                level=params$conf.level, digits=params$digits)                                                        
        #
        # generate forest plot 
        #
        forest_path <- "./r_tmp/forest.png"
        plotData <- create.plot.data.binary(binaryData, params, res)
        forest.plot(plotData, outpath=forest_path)
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("forest plot"=forest_path)
        plot_names <- c("forest plot"="forest_plot")
        modelDesc <- paste("Fixed-Effects Model - Mantel Haenszel(k = ", res$k, ")", sep="")
        hetTest <- "  Test for Heterogeneity:"       
        modelStdLabel <- "  Model Results (standard scale):"
        modelLogLabel <- "  Model Results (log scale):"
        summaryDisp <- list("digits"=params$digits, "modelDesc"=modelDesc, "hetTest"=hetTest, "modelStdLabel"=modelStdLabel, 
                            "modelLogLabel"=modelLogLabel, "df"=res$k.yi - 1, "QE"=res$QE, "QEp"=res$QEp, "estimate"=res$b, 
                            "SE"=res$se, "ZVal"=res$zval, "PVal"=res$pval, "LowerBound"=res$ci.lb, "UpperBound"=res$ci.ub)
        class(summaryDisp) <- "summaryDisplay"                        
        results <- list("images"=images, "summary"=summary, "plot_names"=plot_names)
    }
    results
}
                                
binary.fixed.mh.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    apply_adjustment_to = c("only0", "all")
    
    params <- list("measure"=binary_metrics, "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("measure"="OR", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("measure", "conf.level", "digits", "adjust", "to")
    
    # constraints
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.mh.is.feasible <- function(binaryData){
    # only feasible if we have raw (2x2) data for all studies
    length(binaryData@g1O1)==length(binaryData@g1O2) &&
    length(binaryData@g1O2)==length(binaryData@g2O1) &&
    length(binaryData@g2O1)==length(binaryData@g2O2) &&
         length(binaryData@g1O1) > 0
}

binary.fixed.mh.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$summary
    overall <- list(c("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub))
    overall
}
                                                                                                                         
##################################################
#       binary fixed effects -- Peto             #
##################################################
binary.fixed.peto <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  

    if (length(binaryData@g1O1) == 1){
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list("summary"=res)
    }
    else{  
        res <- rma.peto(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                                level=params$conf.level, digits=params$digits)              
                                                  
        #
        # generate forest plot 
        #
        forest_path <- "./r_tmp/forest.png"
        png(forest_path)
        forest_plot<-forest.rma(res, digits=params$digits)
        dev.off()
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("forest plot"=forest_path)
        plot_names <- c("forest plot"="forest_plot")
        modelDesc <- paste("Fixed-Effects Model - Peto (k = ", res$k, ")", sep="")
        hetTest <- "Test for Heterogeneity:"       
        modelStdLabel <- "Model Results (standard scale):"
        modelLogLabel <- "Model Results (log scale):"
        summaryDisp <- list("digits"=params$digits, "modelDesc"=modelDesc, "hetTest"=hetTest, "modelStdLabel"=modelStdLabel, 
                            "modelLogLabel"=modelLogLabel, "df"=res$k - res$p, "QE"=res$QE, "QEp"=res$QEp, "estimate"=res$b, 
                            "SE"=res$se, "ZVal"=res$zval, "PVal"=res$pval, "LowerBound"=res$ci.lb, "UpperBound"=res$ci.ub)
        class(summaryDisp) <- "summaryDisplay"                        
        results <- list("images"=images, "summary"=summary, "plot_names"=plot_names)
    }
    results
}

                                
binary.fixed.peto.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list( "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5)
    
    var_order = c("conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults,  "var_order"=var_order)
}

binary.fixed.peto.is.feasible <- function(binaryData){
    # only feasible if we have raw (2x2) data for all studies
    length(binaryData@g1O1)==length(binaryData@g1O2) &&
    length(binaryData@g1O2)==length(binaryData@g2O1) &&
    length(binaryData@g2O1)==length(binaryData@g2O2) &&
         length(binaryData@g1O1) > 0
}

binary.fixed.peto.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$summary
    overall <- list(c("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub))
    overall
}


##################################
#  binary random effects         #
##################################
binary.random <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")
    
    results <- NULL
    if (length(binaryData@g1O1) == 1 || length(binaryData@y) == 1){
        res <- get.res.for.one.binary.study(binaryData, params)
        results <- list("summary"=res)
    }
    else{     
        # call out to the metafor package
        if (length(binaryData@g1O1) > 0) {
            res<-rma.uni(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                        ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                                        method=params$rm.method, measure=params$measure,
                                        level=params$conf.level, digits=params$digits)
        }
        else{
           res<-rma.uni(yi=binaryData@y, sei=binaryData@SE, 
                                        slab=binaryData@studyNames,
                                        method=params$rm.method, level=params$conf.level,
                                        digits=params$digits)
        }
        
        #
        # generate forest plot 
        #
        getwd()
        forest_path <- "./r_tmp/forest.png"
        png(forest_path)
        forest.rma(res, digits=params$digits)
        dev.off()
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("forest plot"=forest_path)
        plot_names <- c("forest plot"="forest_plot")
        
        results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
    }
    results
}


binary.random.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "measure"=binary_metrics, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "measure"="OR", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "measure", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$summary
    overall <- list(c("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub))
    overall
}





