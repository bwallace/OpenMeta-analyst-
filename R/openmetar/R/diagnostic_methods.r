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

compute.diag.point.estimates <- function(diagnostic.data) {
# Computes point estimates based on raw data and adds them to diagnostic.data
    TP <- diagnostic.data@TP
    FN <- diagnostic.data@FN  
    TN <- diagnostic.data@TN 
    FP <- diagnostic.data@FP
    
    # sensitivity
    sens <- TP / (TP + FN)
    # specificity
    spec <- TN / (TN + FP)
    # pos. predictive value
    PPV <-  TP / (TP + FP)
    #neg. predictive value
    NPV <-  TN / (TN + FN)
    # accuracy
    Acc <- (TP + TN) / (TP + TN + FP + FN)
    # positive likelihood ratio
    PLR <- sens / (1 - spec) 
    # negative likelihood ratio
    NLR <- (1 - sens) / spec
    # diagnostic odds ratio
    DOR <- PLR / NLR
    y <- array(c(diagnostic.transform.f("sens")$calc.scale(sens),
                 diagnostic.transform.f("spec")$calc.scale(spec),
                 diagnostic.transform.f("PPV")$calc.scale(PPV),
                 diagnostic.transform.f("NPV")$calc.scale(NPV),
                 diagnostic.transform.f("Acc")$calc.scale(Acc),
                 diagnostic.transform.f("PLR")$calc.scale(PLR),
                 diagnostic.transform.f("NLR")$calc.scale(NLR),
                 diagnostic.transform.f("DOR")$calc.scale(DOR)),
                 dim=c(length(diagnostic.data@study.names), 8))
    colnames(y) <- c("sens", "spec", "PPV", "NPV", "Acc", "PLR", "NLR", "DOR")             
    diagnostic.data@y <- y
    sens.SE <- sqrt((1 / TP) + (1 / FN)) 
    spec.SE <- sqrt((1 / TN) + (1 / FP))
    PPV.SE <- sqrt((1 / TP) + (1 / FP))
    NPV.SE <- sqrt((1 / TN) + (1 / FN))
    Acc.SE <- sqrt((1 / (TP + TN)) + (1 / (FP + FN)))
    PLR.SE <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP)))
    NLR.SE <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP)))
    DOR.SE <- sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN))
    SE <- array(c(sens.SE, spec.SE, PPV.SE, NPV.SE, Acc.SE, PLR.SE, NLR.SE, DOR.SE),
                dim=c(length(diagnostic.data@study.names), 8))
    colnames(SE) <- c("sens.SE", "spec.SE", "PPV.SE", "NPV.SE", "Acc.SE", "PLR.SE", "NLR.SE", "DOR.SE")
    diagnostic.data@SE <- SE                  
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
diagnostic.fixed <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")

    results <- NULL
    res<-rma.uni(yi=diagnostic.data@y[,params$measure], sei=diagnostic.data@SE[,paste(params$measure,".SE",sep="")], 
                     slab=diagnostic.data@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
    # Create list to display summary of results
    degf <- res$k - res$p
    model.title <- "Fixed-Effect Model - Inverse Variance"
    diagnostic.disp <- create.diagnostic.disp(res, params, degf, model.title)
    # function to pretty-print summary of results.
    results <- list("Summary"=diagnostic.disp)
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
      results <- list("images"=images, "Summary"=diagnostic.disp, "plot_names"=plot.names)
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
    res<-rma.uni(yi=diagnostic.data@y[,params$measure], sei=diagnostic.data@SE[,paste(params$measure,".SE",sep="")], 
                 slab=diagnostic.data@study.names,
                 method=params$rm.method, level=params$conf.level,
                 digits=params$digits)
    #                        
    # Create list to display summary of results
    #
    degf <- res$k.yi - 1
    model.title <- paste("Diagnostic Random-Effects Model (k = ", res$k, ")", sep="")
    summary.disp <- create.diagnostic.disp(res, params, degf, model.title)
 
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