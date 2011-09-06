#######################################
# OpenMeta[Analyst]                   #
# ----                                #
# diagnostic_methods.r                # 
# Facade module; wraps methods        #
# that perform analysis on diagnostic #
# data in a coherent interface.       # 
#######################################

library(metafor)

diagnostic.logit.metrics <- c("Sens", "Spec", "PPV", "NPV", "Acc")
diagnostic.log.metrics <- c("PLR", "NLR", "DOR")

adjust.raw.data <- function(diagnostic.data, params) {
    # adjust raw data by adding a constant to each entry   
    TP <- diagnostic.data@TP
    FN <- diagnostic.data@FN  
    TN <- diagnostic.data@TN 
    FP <- diagnostic.data@FP
    
    if (params$to == "all") {
        TP <- TP + params$adjust
        FN <- FN + params$adjust
        TN <- TN + params$adjust
        FP <- FP + params$adjust
    } else if (params$to == "only0") {
        product <- TP * FN * TN * FP
        # product equals 0 if at least one entry in a row is 0
        TP[product == 0] <- TP[product == 0] + params$adjust
        FN[product == 0] <- FN[product == 0] + params$adjust
        TN[product == 0] <- TN[product == 0] + params$adjust
        FP[product == 0] <- FP[product == 0] + params$adjust
    } else if (params$to == "if0all") {
        if (any(c(TP,FN,TN,FP) == 0)) {
            TP <- TP + params$adjust
            FN <- FN + params$adjust
            TN <- TN + params$adjust
            FP <- FP + params$adjust    
        }
    }
    data.adj <- list("TP"=TP, "FN"=FN, "TN"=TN, "FP"=FP)
}

compute.diag.point.estimates <- function(diagnostic.data, params) {
    # Computes point estimates based on raw data and adds them to diagnostic.data
    data.adj <- adjust.raw.data(diagnostic.data, params)
    terms <- compute.diagnostic.terms(raw.data=data.adj, params)
    metric <- params$measure    
    TP <- data.adj$TP
    FN <- data.adj$FN  
    TN <- data.adj$TN 
    FP <- data.adj$FP
    
    y <- terms$numerator / terms$denominator
      
    diagnostic.data@y <- eval(call("diagnostic.transform.f", params$measure))$calc.scale(y)
 
    diagnostic.data@SE <- switch(metric,
        Sens <- sqrt((1 / TP) + (1 / FN)), 
        Spec <- sqrt((1 / TN) + (1 / FP)),
        PPV <- sqrt((1 / TP) + (1 / FP)),
        NPV <- sqrt((1 / TN) + (1 / FN)),
        Acc <- sqrt((1 / (TP + TN)) + (1 / (FP + FN))),
        PLR <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
        NLR <- sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
        DOR <- sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN)))

    diagnostic.data
}

compute.diagnostic.terms <- function(raw.data, params) { 
    # compute numerator and denominator of diagnostic point estimate.
    metric <- params$measure
    TP <- raw.data$TP
    FN <- raw.data$FN  
    TN <- raw.data$TN 
    FP <- raw.data$FP
    numerator <- switch(metric,
        # sensitivity
        Sens = TP, 
        # specificity
        Spec = TN,
        # pos. predictive value
        PPV =  TP,
        #neg. predictive value
        NPV =  TN,
        # accuracy
        Acc = TP + TN,
        # positive likelihood ratio
        PLR = TP * (TN + FP), 
        # negative likelihood ratio
        NLR = FN * (TN + FP),
        # diagnostic odds ratio
        DOR = TP * TN)
        
    denominator <- switch(metric,
        # sensitivity
        Sens = TP + FN, 
        # specificity
        Spec = TN + FP,
        # pos. predictive value
        PPV =  TP + FP,
        #neg. predictive value
        NPV =  TN + FN,
        # accuracy
        Acc = TP + TN + FP + FN,
        # positive likelihood ratio
        PLR = FP * (TP + FN), 
        # negative likelihood ratio
        NLR = TN * (TP + FN),
        # diagnostic odds ratio
        DOR = FP * FN)  

    terms <- list("numerator"=numerator, "denominator"=denominator)      
}

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

get.res.for.one.diag.study <- function(diagnostic.data, params){
    # this method can be called when there is only one study to 
    # get the point estimate and lower/upper bounds.
    
    ######
    ## Do not check here if the object is NA; we want to recompute the 
    ## data here regardless, and the program will throwup on this check if 
    ## the y estimate doesn't exist on the object.
    #####
    diagnostic.data <- compute.diag.point.estimates(diagnostic.data, params)
    
    y <- diagnostic.data@y
    se <- diagnostic.data@SE

    # note: conf.level is given as, e.g., 95, rather than .95.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    ub <- y + mult*se
    lb <- y - mult*se
    # we make lists to comply with the get.overall method
    res <- list("b"=c(y), "ci.lb"=lb, "ci.ub"=ub, "se"=se) 
    res
}

logit <- function(x) {
	log(x/(1-x))
}

invlogit <- function(x) {
	exp(x) / (1 + exp(x))
}

###################################################
#     multiple diagnostic fixed effects           #
###################################################

multiple.diagnostic <- function(fname, diagnostic.data, params) {
    # wrapper for applying single.diagnostic.fixed for multiple metrics    
    metrics <- c()
    results <- list()
    for (count in 1:length(params)) {
        metrics <- c(metrics, params[[count]]$measure)
        if (params[[count]]$measure=="Sens") {
            sens.index <- count
        }
        if (params[[count]]$measure=="Spec") {
            spec.index <- count
        }
        if (params[[count]]$measure=="PLR") {
            plr.index <- count
        }
        if (params[[count]]$measure=="NLR") {
            nlr.index <- count
        }
    }
    
    metrics.reduced <- metrics
    images <- c()
    plot.names <- c()
    if (("Sens" %in% metrics) & ("Spec" %in% metrics)) {
        results.sens.spec <- NULL
        results.sens.spec <- side.by.side.plots(diagnostic.data, params.left=params[[sens.index]], params.right=params[[spec.index]])
        images <- c(results.sens.spec$images)
        plot_names <- c(results.sens.spec$plot_names)
        params[[sens.index]]$create.plot <- FALSE
        params[[spec.index]]$create.plot <- FALSE
        # Don't create individual forest plots for sens and spec if both are checked.
        metrics.reduced <- setdiff(metrics, c("Sens", "Spec"))
        #summaries.sens.spec <- c(results$"Sensitivity Summary", results$"Specificity Summary")
        #results$"Specificity_Summary" <- results.sens.spec$"Specificity Summary"
        #results$"Specificity_Summary" <- results.sens.spec$"Specificity Summary"
    }
    
    if ("PLR" %in% metrics) {
        results.plr.nlr <- NULL
        results.plr.nlr <- side.by.side.plots(diagnostic.data, params.left=params[[plr.index]], params.right=params[[nlr.index]])
        images <- c(images, results.plr.nlr$images)
        plot.names <- c(plot.names, results.plr.nlr$plot_names)
        params[[plr.index]]$create.plot <- FALSE
        params[[nlr.index]]$create.plot <- FALSE
        # Don't create individual forest plots for plr and nlr if plr is checked.
        metrics.reduced <- setdiff(metrics.reduced, c("PLR", "NLR"))
        #results$"Positive_Likelihood_Ratio_Summary" <- results.plr.nlr$"PLR Summary"
        #results$"Negative_Likelihood_Ratio_Summary" <- results.plr.nlr$"NLR Summary"
        
    }
    pretty.names <- diagnostic.fixed.pretty.names()
    for (count in 1:length(params)) {
        results.tmp <- eval(call(fname, diagnostic.data, params[[count]]))
        #results.tmp <- diagnostic.fixed.inv.var(diagnostic.data, params=params[[count]])
        images <- c(images, results.tmp$images)
        plot.names <- c(plot.names, results.tmp$plot_names)
        results[[count]] <- results.tmp
        names(results[count]) <- eval(parse(text=paste("pretty.names$measure$",params[[count]]$measure,sep="")))
    }
    results$images <- images
    results$plot_names <- plot.names
    results
}

multiple.diagnostic.params <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")

    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)

    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")

    var_order = c("conf.level", "digits", "adjust", "to")

    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}


###################################################
#            diagnostic fixed effects             #
###################################################
diagnostic.fixed.inv.var <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    results <- NULL
    if (length(diagnostic.data@TP) == 1 || length(diagnostic.data@y) == 1){
        res <- get.res.for.one.diag.study(diagnostic.data, params)
        # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    } else {
         # call out to the metafor package
        res<-rma.uni(yi=diagnostic.data@y, sei=diagnostic.data@SE, 
                     slab=diagnostic.data@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
         # Create list to display summary of results
        model.title <- paste("Diagnostic Fixed-Effects Model - Inverse Variance (k = ", res$k, ")", sep="")
        data.type <- "diagnostic"
        summary.disp <- create.summary.disp(res, params, model.title, data.type)
        pretty.names <- diagnostic.fixed.inv.var.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
          summary.disp$table.titles[count] <- paste(pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
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
          names(results)[2] <- paste(pretty.metric, " Summary", sep="")
       }
        else {
          results <- list("Summary"=summary.disp)
          names(results)[1] <- paste(pretty.metric, " Summary", sep="")
        } 
    }
    
    results
}

diagnostic.fixed.inv.var.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")

    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)

    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")

    var_order = c("conf.level", "digits", "adjust", "to")

    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

diagnostic.fixed.inv.var.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic Fixed-Effects Inverse Variance", 
                         "description" = "Performs fixed-effects meta-analysis with inverse variance weighting.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero."),
                         "measure"=list("Sens"="Sensitivity", "Spec"="Specificity", "DOR"="Odds Ratio", "PLR"="Positive Likelihood Ratio",
                                        "NLR"="Negative Likelihood Ratio")           
                          )
}

diagnostic.fixed.inv.var.is.feasible <- function(diagnostic.data, metric){
    metric %in% c("Sens", "Spec", "PLR", "NLR", "DOR")
}

diagnostic.fixed.inv.var.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}

################################################
#  diagnostic fixed effects -- mantel haenszel #
################################################
diagnostic.fixed.mh <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")  
    results <- NULL
    if (length(diagnostic.data@TP) == 1 || length(diagnostic.data@y) == 1){
        res <- get.res.for.one.diagnostic.study(diagnostic.data, params)
         # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    } 
    else {
        res <- switch(params$measure,
        
            "DOR" = rma.mh(ai=diagnostic.data@TP, bi=diagnostic.data@FN, 
                                ci=diagnostic.data@FP, di=diagnostic.data@TN, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, measure="OR",
                                add=c(params$adjust, 0), to=c(params$to, "none")),
                                
            "PLR" = rma.mh(ai=diagnostic.data@TP, bi=diagnostic.data@FN, 
                                ci=diagnostic.data@FP, di=diagnostic.data@TN, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, measure="RR",
                                add=c(params$adjust, 0), to=c(params$to, "none")),
        
            "NLR" = rma.mh(ai=diagnostic.data@FN, bi=diagnostic.data@TP, 
                                ci=diagnostic.data@TN, di=diagnostic.data@FP, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, measure="RR",
                                add=c(params$adjust, 0), to=c(params$to, "none")))
  
            # if measure is "NLR", switch ai with bi, and ci with di
            # in order to use rma.mh with measure "RR"

        #                        
        # Create list to display summary of results
        #
        model.title <- paste("Diagnostic Fixed-Effects Model - Mantel Haenszel\n\nMetric: ", params$measure, sep="")
        data.type <- "diagnostic"
        summary.disp <- create.summary.disp(res, params, model.title, data.type)
        pretty.names <- diagnostic.fixed.mh.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
          summary.disp$table.titles[count] <- paste(pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
        #
        # generate forest plot
        #
        if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
            if (is.null(diagnostic.data@y) || is.null(diagnostic.data@SE)) {
                diagnostic.data <- compute.diag.point.estimates(diagnostic.data, params)
                # compute point estimates for plot.data in case they are missing
            }
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
            forest.plot(forest.data=plot.data, outpath=forest.path)
    
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
            names(results)[2] <- paste(pretty.metric, " Summary", sep="")
        }
        else {
            results <- list("Summary"=summary.disp)
            names(results)[1] <- paste(pretty.metric, " Summary", sep="")
        }    
    }
    results
}
                                
diagnostic.fixed.mh.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("conf.level", "digits", "adjust", "to")
    
    # constraints
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

diagnostic.fixed.mh.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic Fixed-Effects Mantel Haenszel", 
                         "description" = "Performs fixed-effects meta-analysis using the Mantel Haenszel method.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero."),
                          "measure"=list("Sens"="Sensitivity", "Spec"="Specificity", "DOR"="Odds Ratio", "PLR"="Positive Likelihood Ratio",
                                        "NLR"="Negative Likelihood Ratio")
                          )
}

diagnostic.fixed.mh.is.feasible <- function(diagnostic.data, metric){
    metric %in% c("DOR", "PLR", "NLR")
}

diagnostic.fixed.mh.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}

###################################################
#            create side-by-side forest.plots     #
###################################################

side.by.side.plots <- function(diagnostic.data, params.left, params.right){
    # creates two side-by-side forest plots
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    results <- NULL
    if (length(diagnostic.data@TP) == 1){
        res <- get.res.for.one.diag.study(diagnostic.data, params.left)
        # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    } else {
        params.left$fp_show_col1 <- 'TRUE'
        params.right$fp_show_col1 <- 'FALSE'
        diagnostic.data.left <- compute.diag.point.estimates(diagnostic.data, params.left)
        diagnostic.data.right <- compute.diag.point.estimates(diagnostic.data, params.right)
        res.left<-rma.uni(yi=diagnostic.data.left@y, sei=diagnostic.data.left@SE, 
                     slab=diagnostic.data.left@study.names,
                     method="FE", level=params.left$conf.level,
                     digits=params.left$digits)
        res.right<-rma.uni(yi=diagnostic.data.right@y, sei=diagnostic.data.right@SE, 
                     slab=diagnostic.data.right@study.names,
                     method="FE", level=params.right$conf.level,
                     digits=params.right$digits)             
        
        forest.path <- paste(params.left$fp_outpath, sep="")
        plot.data.left <- create.plot.data.diagnostic(diagnostic.data.left, params.left, res.left)
        plot.data.right <- create.plot.data.diagnostic(diagnostic.data.right, params.right, res.right)
        two.forest.plots(plot.data.left, plot.data.right, outpath=forest.path)
        #
        # Now we package the results in a dictionary (technically, a named
        # vector). In particular, there are two fields that must be returned;
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one
        # of each.
        #
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        results <- list("images"=images, "plot.names"=plot.names)
    }
    results
}

sens.and.spec.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")

    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)

    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")

    var_order = c("conf.level", "digits", "adjust", "to")

    parameters <- Rlist("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

sens.and.spec.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic Fixed-Effects Sensitivity and Specificity", 
                         "description" = "Performs fixed-effects meta-analysis of sensitivity and specificity with inverse variance weighting.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                          )
}

##################################
#  diagnostic random effects     #
##################################
diagnostic.random <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    
    results <- NULL
    if (length(diagnostic.data@TP) == 1 || length(diagnostic.data@y) == 1){
        res <- get.res.for.one.diag.study(diagnostic.data, params)
        # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    } else {
        # call out to the metafor package
        res<-rma.uni(yi=diagnostic.data@y, sei=diagnostic.data@SE, 
                 slab=diagnostic.data@study.names,
                 method=params$rm.method, level=params$conf.level,
                 digits=params$digits)
        #                        
        # Create list to display summary of results
        #

        model.title <- paste("Diagnostic Random-Effects Model (k = ", res$k, ")", sep="")
        data.type <- "diagnostic"
        summary.disp <- create.summary.disp(res, params, model.title, data.type)
        pretty.names <- diagnostic.random.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
          summary.disp$table.titles[count] <- paste(pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
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
            names(results)[2] <- paste(pretty.metric, " Summary", sep="")
        }
        else {
            results <- list("Summary"=summary.disp)
            names(results)[1] <- paste(pretty.metric, " Summary", sep="")
        } 
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

diagnostic.random.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic Random-Effect", 
                         "description" = "Performs random-effects meta-analysis.",
                         "rm.method"=list("pretty.name"="Random method", "description"="Method for estimating between-studies heterogeneity"),                      
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero."),
                         "measure"=list("Sens"="Sensitivity", "Spec"="Specificity", "DOR"="Odds Ratio", "PLR"="Positive Likelihood Ratio",
                                        "NLR"="Negative Likelihood Ratio")
                         )
}

diagnostic.random.is.feasible <- function(diagnostic.data, metric){
    metric %in% c("Sens", "Spec", "PLR", "NLR", "DOR")      
}
diagnostic.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}

###################################################
#            diagnostic SROC                      #
###################################################
diagnostic.fixed.sroc <- function(diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
        # add constant to zero cells
        data.adj <- adjust.raw.data(diagnostic.data,params)
        # compute true positive ratio = sensitivity 
        TPR <- data.adj$TP / (data.adj$TP + data.adj$FN)
        # compute false positive ratio = 1 - specificity
        FPR <- data.adj$FP / (data.adj$TN + data.adj$FP)
        S <- logit(TPR) + logit(FPR)
        D <- logit(TPR) - logit(FPR)
        s.range <- list("max"=max(S), "min"=min(S))
        if (params$sroc.weighted == "weighted") {
            inv.var <- data.adj$TP + data.adj$FN + data.adj$FP + data.adj$TN
            # compute total number in each study
            res <- lm(D ~ S, weights=inv.var)
           # weighted linear regression
        } else {
           res <- lm(D~S)
           # unweighted regression 
        }
    summary.disp <- "SROC Plot"
    # Create list to display summary of results
    fitted.line <- list(intercept=res$coefficients[1], slope=res$coefficients[2])
    #sroc.path <- paste(params$fp_outpath, sep="")
    sroc.path <- "./r_tmp/sroc.png"
    plot.data <- list("fitted.line" = fitted.line, "TPR"=TPR, "FPR"=FPR, "inv.var" = inv.var, "s.range" = s.range, "weighted"=params$sroc.weighted)
    sroc.plot(plot.data, outpath=sroc.path)
    #
    # Now we package the results in a dictionary (technically, a named
    # vector). In particular, there are two fields that must be returned;
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one
    # of each.
    #
    images <- c("SROC"=sroc.path)
    plot.names <- c("sroc"="sroc")
    results <- list("images"=images, "Summary"=summary.disp, "plot_names"=plot.names)
    results
}

diagnostic.fixed.sroc.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    sroc.weighted <- c("weighted", "unweighted")

    params <- list("conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to, "sroc.weighted"=sroc.weighted)

    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="all", "sroc.weighted"="weighted")

    var_order = c("conf.level", "digits", "adjust", "to", "sroc.weighted")

    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

diagnostic.fixed.sroc.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic fixed SROC", 
                         "description" = "Plots diagonostic SROC.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                          )
}
