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
# params <- list(measure="OR", conf.level=95, digits=3)

library(metafor)

binary.log.metrics <- c("OR", "RR", "PETO")

compute.for.one.bin.study <- function(binary.data, params){
    res <- escalc(params$measure, ai=binary.data@g1O1, bi=binary.data@g1O2, 
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
    rawData <- array(c("Study", binaryData@studyNames, "Events (T)", eventT, "Subjects (T)", subjectT, "Events (C)", eventC, 
                    "Subjects (C)", subjectC, "Effect size", y, "Lower bound", LL, "Upper bound", UL), 
                    dim=c(length(binaryData@studyNames) + 1, 8))
    class(rawData) <- "Table" 
    return(rawData)
}

print.summaryDisplay <- function(results,...) {
    # Prints results summary
    cat(results$modelTitle)
    cat("\n\n")
    if (!is.na(results$hetTestData$Title)) {
        cat(results$hetTestData$Title)
        cat("\n")
        hetTable <- list2Array(results$hetTestData)
        print(hetTable)
        cat("\n")
    }                   
    if (!is.na(results$resultData$Title)) {
        cat(results$resultData$Title)
        cat("\n")
        resultsTable <- list2Array(results$resultData)
        print(resultsTable)
        cat("\n")
    }
    if (!is.na(results$altData$Title)) {
        cat(results$altData$Title)
        cat("\n")
        altTable <- list2Array(results$altData)
        print(altTable)
    }

    #if (length(binaryData@g1O1) > 0) {
    #    rawData<-extractData(binaryData, params)
    #    cat("  Data\n")
    #    print(rawData)
    #}
}

print.regDisplay <- function(regDisp, ...) {
     # Prints regression statistics
     regTable <- list2Array(regDisp)
     print(regTable)
}

list2Array <- function(dataList) {
    # Accepts a named list dataList and converts it to an array in which 
    # first row is the names and second row is the values.
    # dataList$Labels and dataList$Labels must have the same length.
    if (length(dataList$Labels) == length(dataList$Values[1,])) {
      listVals <- dataList$Values
      numValRows <- length(listVals[,1])
      numCols <- length(listVals[1,])
      arr <- array(dim=c(numValRows + 1,numCols))
      arr[1,] <- dataList$Labels
      for (count in 1:numValRows) {
          arr[count+1,] <- listVals[count,]
      }
      class(arr) <- "Table"
    }
    else {
        arr = "Labels list and values list have different lengths."
    }
    return(arr)
}

print.Table <- function(tableData) {
    # Prints an array tableData with lines separating rows and columns.
    #rowLength <- 1
    numRows <- length(tableData[,1])
    numCols <- length(tableData[1,])
    # Compute column widths
    colWidths <- NULL
    for (colIndex in 1:numCols) {
      colWidths <- c(colWidths, max(nchar(tableData[,colIndex])) + 4)
    }
    tableWidth <- sum(colWidths) + numCols + 1 
    # Create line of dashes of length tableWidth - 2
    dashLine <- NULL
    for (count in 1:(tableWidth - 2)) {
        dashLine <- paste(dashLine, "-", sep="")
    }
    topLine <- paste("+", dashLine, "+", sep="")
    middleLine <- paste("|", dashLine, "|", sep="")
      
    # Build table
    cat(topLine)
    cat("\n")
    for (rowIndex in 1:numRows) {
        tableRow <- "|"
        for (colIndex in 1:numCols) {
            colWidth <- colWidths[colIndex]
            entry <- padEntry(tableData[rowIndex,colIndex], colWidth)
            tableRow <- paste(tableRow, entry, "|", sep="")
        }
        cat(tableRow)
        cat("\n")
        if (rowIndex < numRows) {
            cat(middleLine) 
            cat("\n")
        } 
    } 
    cat(topLine)
    cat("\n")
}    

padEntry <- function(entry, colWidth) {
    # Adds spaces to entry so that it will be centered in a column of width colWidth.
    # Pad a table entry with zeros
    for (i in 1:floor((colWidth - nchar(entry))/2)) {
        entry <- paste(" ", entry, sep="")
    }
    for (i in 1:ceiling(colWidth - nchar(entry))/2) {
        entry <- paste(entry, " ", sep="")
    }
    return(entry)
}

roundedDisplay <- function(x, digits) {
    # Prints "< 10^(-digits)" if x is < 10^(-digits) or "x" otherwise
    xDisp <- round(x, digits)
    for (count in 1:length(x)) {
        if (abs(xDisp[count]) < 10^(-digits)) {
          xDisp[count] <- paste("< ", 10^(-digits), sep = "", collapse = "")
        }
    }
    return(xDisp)
} 

createSummaryDisp <- function(res, params, degf, modelTitle) {
    QLabel =  paste("Q(df = ", degf, ")", sep="")
    I2 <- max(0, (res$QE - degf)/res$QE)
    I2 <- paste(100 * round(I2, digits = 2), "%")
    QE <- round(res$QE, digits=params$digits)
    QEp <- roundedDisplay(res$QEp, digits=params$digits)
    hetTestData <- list("Title" = "  Test for Heterogeneity", "Labels" = c(QLabel, "p-Value", "I^2"),
                        "Values" = array(c(QE, QEp, I2), dim=c(1,3)))
    estDisp <- round(binary.transform.f(params$measure)$display.scale(res$b), digits=params$digits) 
    lbDisp <- round(binary.transform.f(params$measure)$display.scale(res$ci.lb), digits=params$digits)
    ubDisp <- round(binary.transform.f(params$measure)$display.scale(res$ci.ub), digits=params$digits)
    
    pVal <- roundedDisplay(res$pval, digits=params$digits)
    zVal <- round(res$zval, digits=params$digits)
    se <- round(res$se, digits=params$digits)
    if (binary.transform.f(params$measure)$display.scale(1)!= binary.transform.f(params$measure)$calc.scale(1)) { 
         resultData <- list("Title" = "  Model Results (reporting scale)",
                        "Labels" = c("Estimate", "p-Value", "Z-Value", "Lower bound", "Upper bound"),
                        "Values" = array(c(estDisp, pVal, zVal, lbDisp, ubDisp), dim=c(1,5)))
         estCalc <- round(res$b, digits=params$digits) 
         lbCalc <- round(res$ci.lb, digits=params$digits)
         ubCalc <- round(res$ci.ub, digits=params$digits)
         altData <- list("Title" = "  Model Results (calculation scale)",
                      "Labels" = c("Estimate", "SE", "Lower bound", "Upper bound"),
                      "Values" = array(c(estCalc, se, lbCalc, ubCalc), dim=c(1,4)))        
    }    
     
    else {
        resultData <- list("Title" = "  Model Results (reporting scale)",
                        "Labels" = c("Estimate", "SE", "p-Value", "Z-Value", "Lower bound", "Upper bound"),
                        "Values" = array(c(estDisp, se, pVal, zVal, lbDisp, ubDisp), dim=c(1,6)))
        altData <- list("Title" = NA)
    }
    summaryDisp <- list("digits" = params$digits, "modelTitle" = modelTitle, "hetTestData" = hetTestData, "resultData" = resultData,
                        "altData" = altData)
    class(summaryDisp) <- "summaryDisplay"                     
    return(summaryDisp)
}

createRegressionDisp <- function(res, params) {
    coeffs <- round(res$b, digits=params$digits)
    pvals <- roundedDisplay(res$pval, digits=params$digits)
    lbs <- round(res$ci.lb, digits=params$digits)
    ubs <- round(res$ci.ub, digits=params$digits)
    regDisp <- list("Labels" = c("", "Estimate", "p-Value", "Lower bound", "Upper bound"),
                      "Values" = array(c("Intercept", "Slope", coeffs[1], coeffs[2], pvals[1], pvals[2],
                      lbs[1], lbs[2], ubs[1], ubs[2]), dim=c(2, 5)))
    class(regDisp) <-  "regDisplay"                
    return(regDisp)
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
        # Create list to display summary of results
        degf <- res$k - res$p
        modelTitle <- paste("Fixed-Effects Model - Inverse Variance (k = ", res$k, ")", sep="")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        summaryDisp
        
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
        results <- list("images"=images, "Summary"=summaryDisp, "plot_names"=plot_names)
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
                                level=params$conf.level, digits=params$digits, measure=params$measure) 
        #                        
        # Create list to display summary of results
        #
        degf <- res$k.yi - 1
        modelTitle <- paste("Fixed-Effects Model - Mantel Haenszel (k = ", res$k, ")", sep="")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        summaryDisp                                                                               
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
        results <- list("images"=images, "Summary"=summaryDisp, "plot_names"=plot_names)
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
        # Create list to display summary of results
        #
        degf <- res$k.yi - 1
        modelTitle <- paste("Fixed-Effects Model - Peto (k = ", res$k, ")", sep="")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        summaryDisp                                                 
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
        
        results <- list("images"=images, "Summary"=summaryDisp, "plot_names"=plot_names)
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
        # Create list to display summary of results
        #
        degf <- res$k.yi - 1
        modelTitle <- paste("Binary Random-Effects Model (k = ", res$k, ")", sep="")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        summaryDisp       
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
        
        results <- list("images"=images, "Summary"=summaryDisp, "plot_names"=plot_names)
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