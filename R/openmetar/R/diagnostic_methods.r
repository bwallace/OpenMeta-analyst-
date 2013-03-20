#######################################
# OpenMeta[Analyst]                   #
# ----                                #
# diagnostic_methods.r                # 
# Facade module; wraps methods        #
# that perform analysis on diagnostic #
# data in a coherent interface.       # 
#######################################

library(metafor)
library(HSROC)
library(graphics)

diagnostic.logit.metrics <- c("Sens", "Spec", "PPV", "NPV", "Acc")
diagnostic.log.metrics <- c("PLR", "NLR", "DOR")
bivariate.methods <- c("diagnostic.hsroc", "diagnostic.bivariate.ml")

adjust.raw.data <- function(diagnostic.data, params) {
    # adjust raw data by adding a constant to each entry   
    TP <- diagnostic.data@TP
    FN <- diagnostic.data@FN  
    TN <- diagnostic.data@TN 
    FP <- diagnostic.data@FP
    
    if ("to" %in% names(params)){
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
      
    diagnostic.data@y <- eval(call("diagnostic.transform.f", params$measure))$calc.scale(y, n)
 
	# logit scale SE
    diagnostic.data@SE <- switch(metric,
        Sens = sqrt((1 / TP) + (1 / FN)), 
        Spec = sqrt((1 / TN) + (1 / FP)),
        PPV = sqrt((1 / TP) + (1 / FP)),
        NPV = sqrt((1 / TN) + (1 / FN)),
        Acc = sqrt((1 / (TP + TN)) + (1 / (FP + FN))),
        PLR = sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
        NLR = sqrt((1 / TP) - (1 / (TP + FN)) + (1 / FP) - (1 / (TN + FP))),
        DOR = sqrt((1 / TP) + (1 / FN) + (1 / FP) + (1 / TN)))
	# display scale SE


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
    display.scale <- function(x, ...){
        if (metric.str %in% diagnostic.log.metrics){
            exp(x)
        } else if (metric.str %in% diagnostic.logit.metrics) {
            invlogit(x)
        } else {
            # identity function
            x
        }
    }
    
    calc.scale <- function(x, ...){
        if (metric.str %in% diagnostic.log.metrics){
            log(x)
        } else if (metric.str %in% diagnostic.logit.metrics){
            logit(x)
        } else {
            # identity function
            x
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

###################################################
#     multiple diagnostic methods                 #
###################################################
multiple.diagnostic <- function(fnames, params.list, diagnostic.data) {

    # wrapper for applying multiple diagnostic functions and metrics    

    ####
    # fnames -- names of diagnostic meta-analytic functions to call
    # params.list -- parameter lists to be passed along to the functions in
    #              fnames
    # diagnostic.data -- the (diagnostic data) that is to be analyzed 
    ###
    metrics <- c()
    results <- list()
    pretty.names <- diagnostic.fixed.inv.var.pretty.names()
    sens.spec.outpath <- c()
    for (count in 1:length(params.list)) {
        metrics <- c(metrics, params.list[[count]]$measure)
        if (params.list[[count]]$measure=="Sens") {
            sens.index <- count
        }
        if (params.list[[count]]$measure=="Spec") {
            spec.index <- count
        }
        if (params.list[[count]]$measure=="PLR") {
            plr.index <- count
        }
        if (params.list[[count]]$measure=="NLR") {
            nlr.index <- count
        }
    }
    
    images <- c()
    image.order <- c()
    plot.names <- c()
    plot.params.paths <- c()
    plot.pdfs.paths <- c() # sometimes we want to just output pdfs at run-time
    remove.indices <- c()

    if (("Sens" %in% metrics) & ("Spec" %in% metrics)) {
        ####
        # we are running an analysis for sens *and* spec;
        # has a bivariate method been selected??
        fname <- fnames[sens.index]
        if (fname %in% bivariate.methods){
            params.sens <- params.list[[sens.index]] # we could pick either here
            biv.results <- eval(call(fname, diagnostic.data, params.sens))
            results <- c(results, biv.results$Summary)
            images <- c(images, biv.results$images)
            image.order <- append.image.order(image.order, biv.results)
            remove.indices <- c(sens.index, spec.index)
        } else {
            ###
            # we're not running bivariate; proceed as usual
            # create side-by-side forest plots for sens and spec.
            params.sens <- params.list[[sens.index]]
            params.spec <- params.list[[spec.index]]
            params.sens$create.plot <- FALSE
            params.spec$create.plot <- FALSE
            params.tmp <- list("left"=params.sens, "right"=params.spec)
            
            diagnostic.data.sens <- compute.diag.point.estimates(diagnostic.data, params.sens)
            diagnostic.data.spec <- compute.diag.point.estimates(diagnostic.data, params.spec)
            diagnostic.data.all <- list("left"=diagnostic.data.sens, "right"=diagnostic.data.spec)
            
            results.sens <- eval(call(fname, diagnostic.data.sens, params.sens))
            results.spec <- eval(call(fname, diagnostic.data.spec, params.spec))
            summary.sens <- list("Summary"=results.sens$Summary)
            names(summary.sens) <- paste(eval(parse(text=paste("pretty.names$measure$", params.sens$measure,sep=""))), " Summary", sep="")
            summary.spec <- list("Summary"=results.spec$Summary)
            names(summary.spec) <- paste(eval(parse(text=paste("pretty.names$measure$", params.spec$measure,sep=""))), " Summary", sep="")
            results <- c(results, summary.sens, summary.spec)
            
            res.sens <- results.sens$Summary$MAResults
            res.spec <- results.spec$Summary$MAResults
            res <- list("left"=res.sens, "right"=res.spec)
            plot.data <- create.side.by.side.plot.data(diagnostic.data.all, params=params.tmp, res=res)
            forest.path <- paste(params.sens$fp_outpath, sep="")
            two.forest.plots(plot.data, outpath=forest.path)
               
            forest.plot.params.path <- save.data(om.data=diagnostic.data.all, res, params=params.tmp, plot.data)
            plot.params.paths.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.plot.params.path)
            plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
            images.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.path)
            images <- c(images, images.tmp)
            image.order <- c(image.order, "Sensitivity and Specificity Forest Plot")
            plot.names.tmp <- c("forest plot"="forest.plot")
            plot.names <- c(plot.names, plot.names.tmp)
            
            # create SROC plot
            sroc.path <- "./r_tmp/roc.png"
            sroc.plot.data <- create.sroc.plot.data(diagnostic.data, params=params.sens)
            sroc.plot(sroc.plot.data, sroc.path)
            # we use the system time as our unique-enough string to store
            # the params object
            sroc.plot.params.path <- save.plot.data(sroc.plot.data)
            plot.params.paths.tmp <- c("SROC"=sroc.plot.params.path)
            plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
            images.tmp <- c("SROC"=forest.path)
            images <- c(images, c("SROC"=sroc.path))
            image.order <- c(image.order, "SROC")
            plot.names <- c(plot.names, c("sroc"="sroc"))
            remove.indices <- c(sens.index, spec.index)
        }
    }
    
    if (("NLR" %in% metrics) & ("PLR" %in% metrics)) {
        # create side-by-side forest plots for NLR and PLR.
        params.nlr <- params.list[[nlr.index]]
        params.plr <- params.list[[plr.index]]
        params.nlr$create.plot <- FALSE
        params.plr$create.plot <- FALSE
        params.tmp <- list("left"=params.nlr, "right"=params.plr)
        
        fname <- fnames[nlr.index]
        diagnostic.data.nlr <- compute.diag.point.estimates(diagnostic.data, params.nlr)
        diagnostic.data.plr <- compute.diag.point.estimates(diagnostic.data, params.plr)
        diagnostic.data.all <- list("left"=diagnostic.data.nlr, "right"=diagnostic.data.plr)
        
        results.nlr <- eval(call(fname, diagnostic.data.nlr, params.nlr))
        results.plr <- eval(call(fname, diagnostic.data.plr, params.plr))
        summary.nlr <- list("Summary"=results.nlr$Summary)
        names(summary.nlr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.nlr$measure,sep=""))), " Summary", sep="")
        summary.plr <- list("Summary"=results.plr$Summary)
        names(summary.plr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.plr$measure,sep=""))), " Summary", sep="")
        results <- c(results, summary.nlr, summary.plr)
        
        res.nlr <- results.nlr$Summary$MAResults
        res.plr <- results.plr$Summary$MAResults
        res <- list("left"=res.nlr, "right"=res.plr)
        
        plot.data <- create.side.by.side.plot.data(diagnostic.data.all, res=res, params.tmp)
        
        forest.path <- paste(params.nlr$fp_outpath, sep="")
        two.forest.plots(plot.data, outpath=forest.path)
           
        forest.plot.params.path <- save.data(diagnostic.data, res, params=params.tmp, plot.data)
        plot.params.paths.tmp <- c("NLR and PLR Forest Plot"=forest.plot.params.path)
        plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
               
        images.tmp <- c("NLR and PLR Forest Plot"=forest.path)
        image.order <- c(image.order, "NLR and PLR Forest Plot")
        images <- c(images, images.tmp)
        
        plot.names.tmp <- c("forest plot"="forest.plot")
        plot.names <- c(plot.names, plot.names.tmp)
        
        remove.indices <- c(remove.indices, nlr.index, plr.index)
    }

    # remove fnames and params for side-by-side plots
    fnames <- fnames[setdiff(1:length(fnames), remove.indices)]
    params.list <- params.list[setdiff(1:length(params.list), remove.indices)]

    if (length(params.list) > 0) {
        for (count in 1:length(params.list)) {
            # create ma summaries and single (not side-by-side) forest plots.
            #pretty.names <- eval(call(paste(fnames[count],".pretty.names",sep="")))
            diagnostic.data.tmp <- compute.diag.point.estimates(diagnostic.data, params.list[[count]])
            results.tmp <- eval(call(fnames[count], diagnostic.data.tmp, params.list[[count]]))
            images.tmp <- results.tmp$images
            names(images.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
            images <- c(images, images.tmp)
            image.order <- c(image.order, names(images.tmp))
            plot.params.paths.tmp <- results.tmp$plot_params_paths
            names(plot.params.paths.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$", params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
            plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
            plot.names <- c(plot.names, results.tmp$plot_names)
            summary.tmp <- list("Summary"=results.tmp$Summary)
            names(summary.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Summary", sep="")
            results <- c(results, summary.tmp)
        }
    }

    graphics.off()
    results <- c(results, list("images"=images, "image_order"=image.order, "plot_names"=plot.names, 
                               "plot_params_paths"=plot.params.paths))
    results
}

append.image.order <- function(image.order, results){
    if ("image_order" %in% names(results)){
        image.order <- c(image.order, results[["image_order"]])
    } else{
        # just keep the current order
        image.order <- c(image.order, names(results$images))
    }
    image.order
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
		# GD EXPERIMENTAL#########################
		res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
		res$study.names <- diagnostic.data@study.names
		res$study.years <- diagnostic.data@years
		#########################################
        # Create list to display summary of results
        model.title <- paste("Diagnostic Fixed-Effect Model - Inverse Variance (k = ", res$k, ")", sep="")
        summary.disp <- create.summary.disp(diagnostic.data, params, res, model.title)
        pretty.names <- diagnostic.fixed.inv.var.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
          summary.disp$table.titles[count] <- paste(" ", pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
        # Write results to csv file
        if ((is.null(params$write.to.file)) || params$write.to.file == TRUE) {
            results.path <- paste("./r_tmp/diag_fixed_inv_var_", params$measure, "_results.csv", sep="")
            # @TODO Pass in results.path via params
            write.results.to.file(diagnostic.data, params, res, outpath=results.path) 
        }
        if ((is.null(params$create.plot)) || params$create.plot == TRUE) {
            # A forest plot will be created unless
            # params.create.plot is set to FALSE.
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
            changed.params <- plot.data$changed.params
            # list of changed params values
            params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
            changed.params <- c(changed.params, params.changed.in.forest.plot)
            params[names(changed.params)] <- changed.params
            # dump the forest plot params to disk; return path to
            # this .Rdata for later use
            forest.plot.params.path <- save.data(diagnostic.data, res, params, plot.data)

            plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
            images <- c("Forest Plot"=forest.path)
            plot.names <- c("forest plot"="forest_plot")

            results <- list("images"=images, "Summary"=summary.disp, 
                          "plot_names"=plot.names, 
                          "plot_params_paths"=plot.params.paths)
        } else {
            results <- list("Summary"=summary.disp)
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
    pretty.names <- list("pretty.name"="Diagnostic Fixed-Effect Inverse Variance", 
                         "description" = "Performs fixed-effect meta-analysis with inverse variance weighting.",
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
                                add=c(params$adjust, 0), to=c(as.character(params$to), "none")),
                                
            "PLR" = rma.mh(ai=diagnostic.data@TP, bi=diagnostic.data@FN, 
                                ci=diagnostic.data@FP, di=diagnostic.data@TN, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, measure="RR",
                                add=c(params$adjust, 0), to=c(as.character(params$to), "none")),
        
                      # For "NLR", switch ai with bi, and ci with di
                      # in order to use rma.mh with measure "RR"          
            "NLR" = rma.mh(ai=diagnostic.data@FN, bi=diagnostic.data@TP, 
                                ci=diagnostic.data@TN, di=diagnostic.data@FP, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, measure="RR",
                                add=c(params$adjust, 0), to=c(as.character(params$to), "none")))
         
		# GD EXPERIMENTAL#########################
		res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
		res$study.names <- diagnostic.data@study.names
		res$study.years <- diagnostic.data@years
		#########################################		
        #                        
        # Create list to display summary of results
        #
        model.title <- "Diagnostic Fixed-Effect Model - Mantel Haenszel"
        summary.disp <- create.summary.disp(diagnostic.data, params, res, model.title)
        pretty.names <- diagnostic.fixed.mh.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
          summary.disp$table.titles[count] <- paste(" ", pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
        # Write results to csv file
        if ((is.null(params$write.to.file)) || params$write.to.file == TRUE) {
            results.path <- paste("./r_tmp/diag_fixed_mh_", params$measure, "_results.csv", sep="")
            # @TODO Pass in results.path via params
            write.results.to.file(diagnostic.data, params, res, outpath=results.path) 
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
            changed.params <- plot.data$changed.params
            # list of changed params values
            params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
            changed.params <- c(changed.params, params.changed.in.forest.plot)
            params[names(changed.params)] <- changed.params
            # dump the forest plot params to disk; return path to
            # this .Rdata for later use
            forest.plot.params.path <- save.data(diagnostic.data, res, params, plot.data)
            
            plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
            images <- c("Forest Plot"=forest.path)
            plot.names <- c("forest plot"="forest_plot")
            results <- list("images"=images, "Summary"=summary.disp, 
                            "plot_names"=plot.names, 
                            "plot_params_paths"=plot.params.paths)
        }
        else {
            results <- list("Summary"=summary.disp)
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
    pretty.names <- list("pretty.name"="Diagnostic Fixed-Effect Mantel Haenszel", 
                         "description" = "Performs fixed-effect meta-analysis using the Mantel Haenszel method.",
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

##################################################
#       diagnostic fixed effects -- Peto             #
##################################################
diagnostic.fixed.peto <- function(diagnostic.data, params){
  # assert that the argument is the correct type
  if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.") 
  
  if (length(diagnostic.data@TP) == 1 || length(diagnostic.data@y) == 1){
    res <- get.res.for.one.diagnostic.study(diagnostic.data, params)
    # Package res for use by overall method.
    summary.disp <- list("MAResults" = res) 
    results <- list("Summary"=summary.disp)
  }
  else{  
    res <- rma.peto(ai=diagnostic.data@TP, bi=diagnostic.data@FN, 
                    ci=diagnostic.data@FP, di=diagnostic.data@TN, slab=diagnostic.data@study.names,
                    level=params$conf.level, digits=params$digits,
                    add=c(params$adjust, 0), to=c(as.character(params$to), "none"))
	# GD EXPERIMENTAL#########################
	res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
	res$study.names <- diagnostic.data@study.names
	res$study.years <- diagnostic.data@years
	#########################################			
			
    # Corrected values for y and SE
    diagnostic.data@y <- res$yi
    diagnostic.data@SE <- sqrt(res$vi)
    
    #                        
    # Create list to display summary of results
    #
    model.title <- "Diagnostic Fixed-Effect Model - Peto"
    summary.disp <- create.summary.disp(diagnostic.data, params, res, model.title)
    pretty.names <- diagnostic.fixed.peto.pretty.names()
    pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
    for (count in 1:length(summary.disp$table.titles)) {
      summary.disp$table.titles[count] <- paste(" ", pretty.metric, " -", summary.disp$table.titles[count], sep="")
    }
    
    if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
      if (is.null(diagnostic.data@y) || is.null(diagnostic.data@SE)) {
        # compute point estimates for plot.data in case they are missing
        diagnostic.data <- compute.bin.point.estimates(diagnostic.data, params)
      }
      if (is.null(params$write.to.file)) {
        # Write results and study data to csv files  
        res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
        results.path <- paste("./r_tmp/diagnostic_fixed_peto_results.csv")
        # @TODO Pass in results.path via params
        #data.path <- paste("./r_tmp/diagnostic_fixed_peto_study_data.csv")
        write.results.to.file(diagnostic.data, params, res, outpath=results.path)
      }
      if (is.null(params$create.plot)) {
        # Create forest plot and list to display summary of results
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- "Diagnostic Fixed-Effect Model - Peto\n\nMetric: Odds Ratio"
        # Create results display tables
        summary.disp <- create.summary.disp(diagnostic.data, params, res, model.title)
        #
        # generate forest plot 
        #
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
        changed.params <- plot.data$changed.params
        # list of changed params values
        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
        changed.params <- c(changed.params, params.changed.in.forest.plot)
        params[names(changed.params)] <- changed.params
        # dump the forest plot params to disk; return path to
        # this .Rdata for later use
        forest.plot.params.path <- save.data(diagnostic.data, res, params, plot.data)
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        results <- list("images"=images, "Summary"=summary.disp, 
                        "plot_names"=plot.names, "plot_params_paths"=plot.params.paths)
      }
    }
    else {
      results <- list("Summary"=res)
    }    
  }
  results
}

diagnostic.fixed.peto.parameters <- function(){
  # parameters
  apply_adjustment_to = c("only0", "all")
  
  params <- list( "conf.level"="float", "digits"="float",
                  "adjust"="float", "to"=apply_adjustment_to)
  
  # default values
  defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
  
  var_order = c("conf.level", "digits", "adjust", "to")
  
  parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

diagnostic.fixed.peto.pretty.names <- function() {
  pretty.names <- list("pretty.name"="Diagnostic Fixed-Effect Peto", 
                       "description" = "Performs fixed-effect meta-analysis using the Peto method.",
                       "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                       "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                       "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                       "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                         )
}

diagnostic.fixed.peto.is.feasible <- function(diagnostic.data, metric){
  # only feasible if we have raw (2x2) data for all studies
  # and the metric is `DOR'
  metric == "DOR" &&
    length(diagnostic.data@TP)==length(diagnostic.data@FN) &&
    length(diagnostic.data@FN)==length(diagnostic.data@FP) &&
    length(diagnostic.data@FP)==length(diagnostic.data@TN) &&
    length(diagnostic.data@TP) > 0
}

diagnostic.fixed.peto.overall <- function(results) {
  # this parses out the overall from the computed result
  res <- results$Summary
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

		# GD EXPERIMENTAL#########################
		weights <- 1 / (res$vi + res$tau2)
        res$study.weights <- weights / sum(weights)
		res$study.names <- diagnostic.data@study.names
		res$study.years <- diagnostic.data@years
		#########################################
		 
        # Create list to display summary of results
        model.title <- paste("Diagnostic Random-Effects Model (k = ", res$k, ")", sep="")
        summary.disp <- create.summary.disp(diagnostic.data, params, res, model.title)
        pretty.names <- diagnostic.random.pretty.names()
        pretty.metric <- eval(parse(text=paste("pretty.names$measure$", params$measure,sep="")))
        for (count in 1:length(summary.disp$table.titles)) {
            summary.disp$table.titles[count] <- paste(pretty.metric, " -", summary.disp$table.titles[count], sep="")
        }
        # Write results and study data to csv files
        if ((is.null(params$write.to.file)) || params$write.to.file == TRUE) {
            results.path <- paste("./r_tmp/diag_random_", params$measure, "_results.csv", sep="")
            # @TODO Pass in results.path via params
            write.results.to.file(diagnostic.data, params, res, outpath=results.path)
        }
        #
        # generate forest plot 
        #
        if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
            changed.params <- plot.data$changed.params
            # list of changed params values
            params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
            changed.params <- c(changed.params, params.changed.in.forest.plot)
            params[names(changed.params)] <- changed.params
            # update params values
            # we use the system time as our unique-enough string to store
            # the params object
            forest.plot.params.path <- save.data(diagnostic.data, res, params, plot.data)
            
            plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
            images <- c("Forest Plot"=forest.path)
            plot.names <- c("forest plot"="forest_plot")
            results <- list("images"=images, "Summary"=summary.disp, 
                            "plot_names"=plot.names, 
                            "plot_params_paths"=plot.params.paths)
        }
        else {
            results <- list("Summary"=summary.disp)
        } 
    } 
    results
}

diagnostic.random.parameters <- function(){
    apply.adjustment.to = c("only0", "all")
    rm.method.ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm.method.ls, "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply.adjustment.to)
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3,  
                            "adjust"=.5, "to"="only0")
    
    var.order <- c("rm.method", "conf.level", "digits", "adjust", "to")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var.order)
}

diagnostic.random.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Diagnostic Random-Effects", 
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

##################################
#       diagnostic hsroc         #
##################################
diagnostic.hsroc <- function(diagnostic.data, params){
    library(HSROC)
    prev.working.dir <- getwd()

    # step into r_tmp
    setwd("r_tmp")

    ####
    # first we create a unique directory
    unique.name <- as.character(as.numeric(Sys.time()))
    out.dir <- paste(getwd(), unique.name, sep="/")
    dir.create(out.dir)

    #### 
    # convert the diagnostic data to a format consumable
    # by the HSROC lib, this means a data frame
    # with the following columns:
    #    ++ +- -+  --
    diag.data.frame <- 
        data.frame(TP=diagnostic.data@TP, FP=diagnostic.data@FP, FN=diagnostic.data@FN, TN=diagnostic.data@TN)

    ### set up and run the three chains
    chain.out.dirs <- c()
    for (chain.i in 1:params$num.chains){
        chain.out.dir <- paste(out.dir, "/chain_", chain.i, sep="")
        dir.create(chain.out.dir)
        setwd(chain.out.dir)

        # TODO parameterize lambda, theta priors
        res <- try(HSROC(data=diag.data.frame, iter.num=params$num.iters, 
                prior_LAMBDA=c(params$lambda.lower, params$lambda.upper), 
                prior_THETA=c(params$theta.lower, params$theta.upper), 
                path=chain.out.dir))

        # Put in try block in case HSROC fails
        if (class(res)=="try-error") {
            stop("Sorry -- HSROC failed during sampling. Perhaps try running it again?")
        }
        chain.out.dirs <- c(chain.out.dirs, chain.out.dir)
    }

    hsroc.sum <- HSROCSummary(data=diag.data.frame , burn_in=params$burn.in, Thin=params$thin, print_plot=T ,
             path=out.dir, chain=chain.out.dirs )

    #### 
    # pull out the summary
    summary <- c(hsroc.sum[1], hsroc.sum[2])

    ####
    # and the images
    images <- list()
    image.list <- hsroc.sum$image.list

    for (img.name in names(image.list)){
        cur.img.name <- image.list[[img.name]]
        image.list[[img.name]] <- paste(out.dir, cur.img.name, sep="/")
    }

    images <- image.list

    # reset the working directory
    setwd(prev.working.dir)

    # we don't want the SROC plot to be mixed in with 
    # the density plots...
    roc.plot.name <- "Summary ROC"
    image.names <- names(images)
    image.order <- append(roc.plot.name, image.names[image.names!=roc.plot.name])
    results <- list("images"=images, "image_order"=image.order, "Summary"=summary)

}


diagnostic.hsroc.parameters <- function(){
    params <- list("num.iters"="float", "burn.in"="float", "thin"="float", 
                        "theta.lower"="float", "theta.upper"="float",
                        "lambda.lower"="float", "lambda.upper"="float",
                        "num.chains"="float")
    
    # default values
    defaults <- list("num.iters"=5000, "burn.in"=1000, "thin"=2, 
                        "theta.lower"=-2, "theta.upper"=2,
                        "lambda.lower"=-2, "lambda.upper"=2,
                        "num.chains"=3)
    
    var.order <- c("num.iters", "burn.in", "thin", "num.chains", 
                    "theta.lower", "theta.upper",
                    "lambda.lower", "lambda.upper")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var.order)
}

diagnostic.hsroc.pretty.names <- function() {
    pretty.names <- list("pretty.name"="HSROC", 
                         "description" = "Hierarchical regression analysis of diagnostic data\n (Rutter and Gatsonis, Statistics in Medicine, 2001).",
                         "num.iters"=list("pretty.name"="Number of Iterations", "description"="Number of iterations to run."),
                         "burn.in"=list("pretty.name"="Burn in", "description"="Number of draws to use for convergence."),
                         "thin"=list("pretty.name"="Thin", "description"="Thinning."),
                         "num.chains"=list("pretty.name"="Number of Chains", "description"="Number of MCMC chains."),
                         "lambda.lower"=list("pretty.name"="prior on lambda (lower)", "description"="Lower value in (uniform) range over expected lambda values."),
                         "lambda.upper"=list("pretty.name"="prior on lambda (upper)", "description"="Upper value in (uniform) range over expected lambda values."),
                         "theta.lower"=list("pretty.name"="prior on theta (lower)", "description"="Lower value in (uniform) range over expected theta values."),
                         "theta.upper"=list("pretty.name"="prior on theta (upper)", "description"="Upper value in (uniform) range over expected theta values.")
                    )
}


diagnostic.hsroc.ml.is.feasible <- function(diagnostic.data, metric){
    # only estimable when we have >= 5 studies
    length(diagnostic.data@TP) > 4
}


##################################
#   diagnostic biviariate        #
##################################
diagnostic.bivariate.ml <- function(diagnostic.data, params){
    library(boot)

    adjusted.counts <- adjust.raw.data(diagnostic.data, params)

    biv.results <- bivariate.dx.test(adjusted.counts$TP, adjusted.counts$FP, adjusted.counts$FN, adjusted.counts$TN)

    
    #### 
    # parse out results -- @TODO make this nicer.
    logit_sens = biv.results[1,1]
    logit_spec = biv.results[1,2]
    se_logit_sens = biv.results[1,3]
    se_logit_spec = biv.results[1,4]
    correlation = biv.results[1,7]

    digits = 4
    sensitivity <- round(inv.logit(logit_sens), digits)
    # hard-coding CI for now 
    sens.low <- round(inv.logit(logit_sens - 1.96*se_logit_sens), digits)
    sens.high <- round(inv.logit(logit_sens + 1.96*se_logit_sens), digits)

    specificity <- round(inv.logit(logit_spec), digits)
    spec.low <- round(inv.logit(logit_spec - 1.96*se_logit_spec), digits)
    spec.high <- round(inv.logit(logit_spec + 1.96*se_logit_spec), digits)

    r <- round(biv.results$correlation, digits)

    report.array <- array(c("", "Sensitivity","Specificity", "Correlation",
                            "Estimate", sensitivity, specificity, r,
                            "Lower bound", sens.low, spec.low, "",
                            "Upper bound", sens.high,spec.high, ""),
                            dim=c(4,4))

    # this makes it pretty-print?
    class(report.array) <- "summary.data"


    # generate the plot
    path.to.roc.plot <- "./r_tmp/bivariate" # just hard-coding for now
    plot.bivariate(biv.results, adjusted.counts$TP, adjusted.counts$FP, 
                                 adjusted.counts$FN, adjusted.counts$TN,
                                 filepath=path.to.roc.plot)

    images <- c("ROC Plot"=path.to.roc.plot)


    results <- list("images"=images, "Summary"=list("Bivariate Summary"=report.array))
}


diagnostic.bivariate.ml.parameters <- function(){
    apply_adjustment_to = c("only0", "all")

    params <- list("adjust"="float", "to"=apply_adjustment_to)

    # default values
    defaults <- list("adjust"=.5, "to"="only0")

    var_order = c("adjust", "to")

    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}


diagnostic.bivariate.ml.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Bivariate (Maximum Likelihood)", 
                         "description" = "Bivariate analysis of sensitivity and specificity \n using maximum likelihood estimate.",
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                        )  
                    
}

diagnostic.bivariate.ml.is.feasible <- function(diagnostic.data, metric){
    # only estimable when we have >= 5 studies
    length(diagnostic.data@TP) > 4
}



##################################
#            SROC Plot           #
##################################
create.sroc.plot.data <- function(diagnostic.data, params){
    # create plot data for an ROC plot.
  
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
    inv.var <- data.adj$TP + data.adj$FN + data.adj$FP + data.adj$TN
    res <- lm(D~S)
    fitted.line <- list(intercept=res$coefficients[1], slope=res$coefficients[2])
    std.err <- summary(res)$sigma
    # residual standard error
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    # multiplier for std.err to get conf. int. bounds
    plot.options <- list()
    plot.options$roc.xlabel <- params$roc_xlabel
    plot.options$roc.ylabel <- params$roc_ylabel
    plot.options$roc.title <- params$roc_title
    # for future use as options from GUI
    plot.data <- list("fitted.line" = fitted.line, "TPR"=TPR, "FPR"=FPR, "std.err"=std.err, "mult"=mult, "inv.var" = inv.var, "s.range" = s.range, "plot.options"=plot.options)
}

###################################################
#            create side-by-side forest.plots     #
###################################################

create.side.by.side.plot.data <- function(diagnostic.data, params, res) {    
    # creates data for two side-by-side forest plots
    params.left <- params$left
    params.right <- params$right
    #params.left$fp_show_col1 <- 'TRUE'
    #params.right$fp_show_col1 <- 'FALSE'
    # only show study names on the left plot
    res.left <- res$left
    res.right <- res$right    
    diagnostic.data.left <- diagnostic.data$left
    diagnostic.data.right <- diagnostic.data$right
    
    plot.data.left <- create.plot.data.diagnostic(diagnostic.data.left, params.left, res.left)
    plot.data.left$options$fp.title <- pretty.metric.name(as.character(params.left$measure))
      
    plot.data.right <- create.plot.data.diagnostic(diagnostic.data.right, params.right, res.right)
    plot.data.right$options$fp.title <- pretty.metric.name(as.character(params.right$measure))
    
    plot.data <- list("left"=plot.data.left, "right"=plot.data.right)
    plot.data
}