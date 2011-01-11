#######################################
# OpenMeta[Analyst]                   #
# ----                                #
# diagnostic_methods.r                # 
# Facade module; wraps methods        #
# that perform analysis on diagnostic #
# data in a coherent interface.       # 
#######################################

library(metafor)

diagnostic.logit.metrics <- c("sens", "spec", "PPV", "NPV", "Acc")
diagnostic.log.metrics <- c("PLR", "NLR", "DOR")

diagnostic.transform.f <- function(metric.str){
    display.scale <- function(x){
        if (metric.str %in% diagnostic.log.metrics){
            exp(x)
        }
        else {
            if (metric.str %in% diagnostic.logit.metrics){
                invlogit(x)
            }
            else {
                # identity function
                x
            }
        }
    }
    
    calc.scale <- function(x){
        if (metric.str %in% diagnostic.log.metrics){
            log(x)
        }
        else {
        	if (metric.str %in% diagnostic.logit.metrics){
                logit(x)
            }
            else {
                # identity function
                x
            }
         }
    }
    list(display.scale = display.scale, calc.scale = calc.scale)
}

compute.diag.point.estimates <- function(diagnostic.data, params) {
# Computes point estimates based on raw data and adds them to diagnostic.data
    metric <- params$measure
    TP <- diagnostic.data@TP
    FN <- diagnostic.data@FN  
    TN <- diagnostic.data@TN 
    FP <- diagnostic.data@FP
    
    y <- switch(metric,
      # sensitivity
      sens = TP / (TP + FN), 
      # specificity
      spec = TN / (TN + FP),
      # pos. predictive value
      PPV =  TP / (TP + FP),
      #neg. predictive value
      NPV =  TN / (TN + FN),
      # accuracy
      Acc = (TP + TN) / (TP + TN + FP + FN),
      # positive likelihood ratio
      PLR = TP * (TN + FP) / (FP * (TP + FN)), 
      # negative likelihood ratio
      NLR = FN * (TN + FP) / (TN * (TP + FN)),
      # diagnostic odds ratio
      DOR = TP * TN / (FP * FN))
      
    diagnostic.data@y <- eval(call("diagnostic.transform.f", params$measure))$calc.scale(y)
 
    diagnostic.data@SE <- switch(metric,
    sens <- sqrt((1 / TP) + (1 / FN)), 
    spec <- sqrt((1 / TN) + (1 / FP)),
    PPV <- sqrt((1 / TP) + (1 / FP)),
    NPV <- sqrt((1 / TN) + (1 / FN)),
    Acc <- sqrt((1 / (TP + TN)) + (1 / (FP + FN))),
    PLR <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
    NLR <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
    DOR <- sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN)))
    
    diagnostic.data
}

logit <- function(x) {
	log(x/(1-x))
}

invlogit <- function(x) {
	exp(x) / (1 + exp(x))
}

###################################################
#            diagnostic fixed effects             #
###################################################
diagnostic.fixed.inv.var <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")

    results <- NULL
    res<-rma.uni(yi=diagnostic.data@y, sei=diagnostic.data@SE, 
                     slab=diagnostic.data@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
    # Create list to display summary of results
    degf <- res$k - res$p
    model.title <- "Fixed-Effect Model - Inverse Variance"
    data.type <- "diagnostic"
    summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)
    # function to pretty-print summary of results.
    if ((is.null(params$create.plot)) || params$create.plot == TRUE) {
      # A forest plot will be created unless
      # params.create.plot is set to FALSE.
      forest.path <- paste(params$fp_outpath, sep="")
      plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
      forest.plot(plot.data, outpath=forest.path)
      #
      # Now we package the results in a dictionary (technically, a named
      # vector). In particular, there are two fields that must be returned;
      # a dictionary of images (mapping titles to image paths) and a list of texts
      # (mapping titles to pretty-printed text). In this case we have only one
      # of each.
      #
      images <- c("Forest Plot"=forest.path)
      plot.names <- c("forest plot"="forest_plot")
      results <- list("images"=images, "Summary"=summary.disp, "plot_names"=plot.names)
    }
    else {
        results <- list("Summary"=summary.disp)
    }   
    results
}

diagnostic.fixed.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")

    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)

    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")

    var_order = c("conf.level", "digits", "adjust", "to")

    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

##################################
#  diagnostic random effects     #
##################################
diagnostic.random <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Binary data expected.")
    
    results <- NULL
    # call out to the metafor package
    res<-rma.uni(yi=diagnostic.data@y, sei=diagnostic.data@SE, 
                 slab=diagnostic.data@study.names,
                 method=params$rm.method, level=params$conf.level,
                 digits=params$digits)
    #                        
    # Create list to display summary of results
    #
    degf <- res$k.yi - 1
    model.title <- paste("Diagnostic Random-Effects Model (k = ", res$k, ")", sep="")
    data.type <- "diagnostic"
    summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)
 
    #
    # generate forest plot 
    #
    if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
        forest.plot(plot.data, outpath=forest.path)
        
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        results <- list("images"=images, "Summary"=summary.disp, "plot_names"=plot.names)
    }
    else {
        results <- list("Summary"=summary.disp)
    }    
    results
}

diagnostic.random.parameters <- function(){
    # parameters
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

diagnostic.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
    overall <- c(res$b[1], res$ci.lb, res$ci.ub)
    overall
}