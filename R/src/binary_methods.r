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

compute.for.one.bin.study <- function(binaryData, params){
    res <- escalc(params$metric, ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                    ci=binaryData@g2O1, di=binaryData@g2O2)
    res                             
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

results.table <- function(binaryData, params){
    # Creates a table to display the raw data in binaryData
    
    # Compute bounds on confidence intervals.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    LL <- exp(binaryData@y - mult*binaryData@SE)
    UL <- exp(binaryData@y + mult*binaryData@SE)
   
    rawData<-c("Study", "", binaryData@studyNames, 
              "Events (T)", "", round(binaryData@g1O1, digits = params$digits), 
              "Subjects (T)", "", round(binaryData@g1O1 + binaryData@g1O2, digits = params$digits),
              "Events (C)", "", round(binaryData@g2O1, digits = params$digits), 
              "Subjects (T)", "", round(binaryData@g2O1 + binaryData@g2O2, digits = params$digits),
              "Effect size", "", round(exp(binaryData@y), digits = params$digits),  
              "Lower bound", "", round(LL, digits = params$digits), 
              "Upper bound", "", round(UL, digits = params$digits))
    a <- array(rawData, dim = c(length(binaryData@studyNames) + 2, 8))
    df <- data.frame(a)
    dt <- format(df, justify = "centre", width = 12)
    print(dt, row.names = FALSE)
}

print.rma.uni <-
function (x, digits = x$digits, showfit = FALSE, signif.legend = FALSE, 
    ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    cat("\n")
    if (x$method == "FE") {
        if (x$int.only) {
            cat("Fixed-Effects Model (k = ", x$k, ")", sep = "")
        }
        else {
            cat("Fixed-Effects with Moderators Model (k = ", 
                x$k, ")", sep = "")
        }
    }
    else {
        if (x$int.only) {
            cat("Random-Effects Model (k = ", x$k, "; ", sep = "")
        }
        else {
            cat("Mixed-Effects Model (k = ", x$k, "; ", sep = "")
        }
        cat("tau^2 estimator: ", x$method, ")", sep = "")
    }
    if (showfit) {
        cat("\n")
        if (x$method == "REML") {
            fs <- c(formatC(x$fit.stats$REML, digits = digits, 
                format = "f"))
            names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        }
        else {
            fs <- c(formatC(x$fit.stats$ML, digits = digits, 
                format = "f"))
            names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        }
        cat("\n")
        print(fs, quote = FALSE, print.gap = 2)
        cat("\n")
    }
    else {
        cat("\n\n")
    }
    if (x$method != "FE") {
        if (x$int.only) {
            if (x$method == "ML" || x$method == "REML") {
                cat("tau^2 (estimate of total amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), " (SE = ", 
                  ifelse(is.na(x$se.tau2), NA, formatC(x$se.tau2, 
                    digits = digits, format = "f")), ")", "\n", 
                  sep = "")
            }
            else {
                cat("tau^2 (estimate of total amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), "\n", sep = "")
            }
            cat("tau (sqrt of the estimate of total heterogeneity): ", 
                ifelse(x$tau2 >= 0, formatC(sqrt(x$tau2), digits = ifelse(x$tau2 <= 
                  .Machine$double.eps * 10, 0, digits), format = "f"), 
                  NA), "\n", sep = "")
            cat("I^2 (% of total variability due to heterogeneity): ", 
                ifelse(is.na(x$I2), NA, formatC(x$I2, digits = 2, 
                  format = "f")), "%", "\n", sep = "")
            cat("H^2 (total variability / within-study variance):   ", 
                ifelse(is.na(x$H2), NA, formatC(x$H2, digits = 2, 
                  format = "f")), sep = "")
        }
        else {
            if (x$method == "ML" || x$method == "REML") {
                cat("tau^2 (estimate of residual amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), " (SE = ", 
                  ifelse(is.na(x$se.tau2), NA, formatC(x$se.tau2, 
                    digits = digits, format = "f")), ")", "\n", 
                  sep = "")
            }
            else {
                cat("tau^2 (estimate of residual amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), "\n", sep = "")
            }
            cat("tau (sqrt of the estimate of residual heterogeneity): ", 
                ifelse(x$tau2 >= 0, formatC(sqrt(x$tau2), digits = ifelse(x$tau2 <= 
                  .Machine$double.eps * 10, 0, digits), format = "f"), 
                  NA), sep = "")
        }
        cat("\n\n")
    }
    if (!is.na(x$QE)) {
        QEp <- x$QEp
        if (QEp > ncutoff) {
            QEp <- paste("=", formatC(QEp, digits = digits, format = "f"))
        }
        else {
            QEp <- paste("< ", cutoff, sep = "", collapse = "")
        }
        if (x$int.only) {
            cat("Test for Heterogeneity:")
            cat("\n")
            hframe <- data.frame(" " = paste("Q(df = ", x$k - x$p, ") = ", 
                                        formatC(x$QE, digits = digits, format = "f"), sep = ""),
                                " " = paste("p-Value ", QEp, sep = ""), check.names = FALSE)
            hDisplay <- format(hframe, justify = "centre", width = 20)
            print(hDisplay, row.names = FALSE)
            cat("\n \n")                    
            #cat("Q(df = ", x$k - x$p, ") = ", formatC(x$QE, digits = digits, 
             #   format = "f"), ", p-val ", QEp, "\n\n", sep = "")
        }
        else {
            cat("Test for Residual Heterogeneity: \n")
            cat("QE(df = ", x$k - x$p, ") = ", formatC(x$QE, 
                digits = digits, format = "f"), ", p-val ", QEp, 
                "\n\n", sep = "")
        }
    }
    QMp <- x$QMp
    if (QMp > ncutoff) {
        QMp <- paste("=", formatC(QMp, digits = digits, format = "f"))
    }
    else {
        QMp <- paste("< ", cutoff, sep = "", collapse = "")
    }
    if (x$p > 1) {
        cat("Test of Moderators (coefficient(s) ", paste(x$btt, 
            collapse = ","), "): \n", sep = "")
        if (!x$knha) {
            cat("QM(df = ", x$m, ") = ", formatC(x$QM, digits = digits, 
                format = "f"), ", p-val ", QMp, "\n\n", sep = "")
        }
        else {
            cat("F(df1 = ", x$m, ", df2 = ", x$k - x$p, ") = ", 
                formatC(x$QM, digits = digits, format = "f"), 
                ", p-val ", QMp, "\n\n", sep = "")
        }
    }
    # metafor doc says: int.only - logical that indicates whether the model only includes an intercept. For MetaAnalyst, will this
    # always be true?
    if (x$int.only) {  
        dframe <- data.frame(" " = c("Estimate", "", round(exp(x$b), digits=digits)), " " = c("SE", "", round(x$se, digits=digits)), 
                            " " = c("z-Value", "", round(x$zval, digits=digits)), " " = c("p-Value", "", round(x$pval, digits=digits)), 
                            " " = c("Lower bound", " ", round(exp(x$ci.lb), digits=digits)), 
                            " " = c("Upper bound", " ", round(exp(x$ci.ub), digits=digits)), check.names = FALSE)
        if (x$knha) {
            dframe[3] <- c("t Value", "", round(x$zval, digits=digits))
        }
        displayResults <- format(dframe, justify = "centre", width = 10)
        #res.table <- formatC(res.table, digits = digits, format = "f")
        #signif <- symnum(x$pval, corr = FALSE, na = FALSE, cutpoints = c(0, 
        #    0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
        #    "*", ".", " "))
        #res.table <- c(formatC(res.table, digits = digits, format = "f"), 
        #    signif)
        #names(res.table)[7] <- ""
        #res.table[4][x$pval > ncutoff] <- formatC(x$pval[x$pval > 
        #    ncutoff], digits = digits, format = "f")
        #res.table[4][x$pval < ncutoff] <- paste("<", cutoff, 
         #   sep = "", collapse = "")
    }
    else {
        res.table <- cbind(x$b, x$se, x$zval, x$pval, x$ci.lb, 
            x$ci.ub)
        dimnames(res.table)[[2]] <- c("estimate", "se", "zval", 
            "pval", "ci.lb", "ci.ub")
        if (x$knha) {
            dimnames(res.table)[[2]][3] <- c("tval")
        }
        signif <- symnum(x$pval, corr = FALSE, na = FALSE, cutpoints = c(0, 
            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
            "*", ".", " "))
        res.table <- cbind(formatC(res.table, digits = digits, 
            format = "f"), signif)
        dimnames(res.table)[[2]][7] <- ""
        res.table[x$pval > ncutoff, 4] <- formatC(x$pval[x$pval > 
            ncutoff], digits = digits, format = "f")
        res.table[x$pval < ncutoff, 4] <- paste("<", cutoff, 
            sep = "", collapse = "")
    }
    cat("Model Results:")
    cat("\n")
    if (x$int.only) {
        print(displayResults, row.names = FALSE)
    }
    else {
        print(res.table, quote = FALSE, justify = "centre", print.gap = 2)
    }
    cat("\n")
    if (signif.legend == TRUE) {
        cat("---\nSignif. codes: ", attr(signif, "legend"), "\n\n")
    }
    invisible()
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
        class(res) = c("print.results") 
                                                  
        # generate the forest plot 
        forest_path <- "./r_tmp/forest.png"
        png(forest_path)
        # forest_plot<-forest.rma(res, digits=params$digits)
        plotData <- create.plot.data(binaryData, params, res)
        forest.plot(plotData, outpath=forest_path)
        # dev.off()
    
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
        class(res) <- c("print.rma.uni", "rma.uni")
        results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
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
        
        results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
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
        
        results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
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





