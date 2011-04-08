####################################
# OpenMeta[Analyst]                #
# ----                             #
# binary_methods.r                 # 
# Facade module; wraps methods     #
# that perform analysis on binary  #
# data in a coherent interface.    # 
####################################

library(metafor)

binary.log.metrics <- c("OR", "RR")

compute.for.one.bin.study <- function(binary.data, params){
    res <- escalc(params$measure, ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                    ci=binary.data@g2O1, di=binary.data@g2O2,
                                    add=params$adjust, to=params$to)
    res                             
}

compute.bin.point.estimates <- function(binary.data, params) {
# Computes point estimates based on raw data and adds them to binary.data.
    res <- compute.for.one.bin.study(binary.data, params)
    binary.data@y <- res$yi
    binary.data@SE <- sqrt(res$vi)
    binary.data
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

get.res.for.one.binary.study <- function(binary.data, params){
    # this method can be called when there is only one study to 
    # get the point estimate and lower/upper bounds.
    y<-NULL
    se<-NULL
    if (is.na(binary.data@y)){
        res <- compute.for.one.bin.study(binary.data, params)    
        y <- res$yi[1]
        se <- sqrt(res$vi[1])
    }
    else{
        y <- binary.data@y[1]
        se <- binary.data@SE[1]
    }
    # note: conf.level is given as, e.g., 95, rather than .95.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    ub <- y + mult*se
    lb <- y - mult*se
    # we make lists to comply with the get.overall method
    res <- list("b"=c(y), "ci.lb"=lb, "ci.ub"=ub, "se"=se) 
    res
}

extract.data <- function(binary.data, params){
    # Extracts data from binary.data into an array and computes bounds on confidence intervals.
    # Compute bounds on confidence intervals.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    LL <- round(exp(binary.data@y - mult*binary.data@SE), digits = params$digits)
    UL <- round(exp(binary.data@y + mult*binary.data@SE), digits = params$digits)
    # Extract the data from binary.data and round
    eventT <- round(binary.data@g1O1, digits = params$digits)
    subjectT <- round(binary.data@g1O1 + binary.data@g1O2, digits = params$digits)
    eventC <- round(binary.data@g2O1, digits = params$digits)
    subjectC <- round(binary.data@g2O1 + binary.data@g2O2, digits = params$digits)
    y <- round(binary.data@y, digits = params$digits) 
    raw.data <- array(c("Study", binary.data@study.names, "Events (T)", eventT, "Subjects (T)", subjectT, "Events (C)", eventC, 
                    "Subjects (C)", subjectC, "Effect size", y, "Lower bound", LL, "Upper bound", UL), 
                    dim=c(length(binary.data@study.names) + 1, 8))
    class(raw.data) <- "Table" 
    return(raw.data)
}

###################################################
# binary fixed effects -- inverse variance        #
###################################################
binary.fixed.inv.var <- function(binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")  
    results <- NULL
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
        # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
        # Create list to display summary of results
        degf <- res$k - res$p
        model.title <- paste("Fixed-Effects Model - Inverse Variance (k = ", res$k, ")", sep="")
        data.type <- "binary"
        summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)

        results <- list("Summary"=summary.disp, "images"=c())
        if ((is.null(params$create.plot)) || params$create.plot == TRUE) {
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.binary(binary.data, params, res)
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

        }
        else {
            results <- list("Summary"=summary.disp)
        }    
    }
    results
}
                                
binary.fixed.inv.var.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list("conf.level"="float", "digits"="float", 
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.inv.var.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}

############################################
#  binary fixed effects -- mantel haenszel #
############################################
binary.fixed.mh <- function(binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")  
    results <- NULL
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{
        res<-rma.mh(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                ci=binary.data@g2O1, di=binary.data@g2O2, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, measure=params$measure,
                                add=params$adjust, to=params$to) 
        #                        
        # Create list to display summary of results
        #
        degf <- res$k.yi - 1
        model.title <- paste("Fixed-Effects Model - Mantel Haenszel (k = ", res$k, ")", sep="")
        data.type <- "binary"
        summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)
        # generate forest plot 
        #
        if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
            binary.data <- compute.bin.point.estimates(binary.data, params)
            # compute point estimates for plot.data in case they are missing
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.binary(binary.data, params, res)
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
        }
        else {
            results <- list("Summary"=summary.disp)
        }    
    }
    results
}
                                
binary.fixed.mh.parameters <- function(){
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

binary.fixed.mh.is.feasible <- function(binary.data){
    # only feasible if we have raw (2x2) data for all studies
    length(binary.data@g1O1)==length(binary.data@g1O2) &&
    length(binary.data@g1O2)==length(binary.data@g2O1) &&
    length(binary.data@g2O1)==length(binary.data@g2O2) &&
         length(binary.data@g1O1) > 0
}

binary.fixed.mh.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}
                                                                                                                         
##################################################
#       binary fixed effects -- Peto             #
##################################################
binary.fixed.peto <- function(binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.") 
    
    params$measure <- "PETO"
    # use metric PETO to compute point estimates for studies 

    if (length(binary.data@g1O1) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{  
        res <- rma.peto(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                ci=binary.data@g2O1, di=binary.data@g2O2, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits)              
        #                        
        # Create list to display summary of results
        #
        degf <- res$k.yi - 1
        model.title <- paste("Fixed-Effects Model - Peto (k = ", res$k, ")", sep="")
        data.type <- "binary"
        summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)
                                            
        #
        # generate forest plot 
        #
        if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
            binary.data <- compute.bin.point.estimates(binary.data, params)
            # compute point estimates for plot.data in case they are missing
            forest.path <- paste(params$fp_outpath, sep="")
            plot.data <- create.plot.data.binary(binary.data, params, res)
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
        }
        else {
            results <- list("Summary"=summary.disp)
        }    
    }
    results
}

                                
binary.fixed.peto.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list( "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.peto.is.feasible <- function(binary.data){
    # only feasible if we have raw (2x2) data for all studies
    length(binary.data@g1O1)==length(binary.data@g1O2) &&
    length(binary.data@g1O2)==length(binary.data@g2O1) &&
    length(binary.data@g2O1)==length(binary.data@g2O2) &&
         length(binary.data@g1O1) > 0
}

binary.fixed.peto.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}


##################################
#  binary random effects         #
##################################
binary.random <- function(binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    
    results <- NULL
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{     
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, 
                     slab=binary.data@study.names,
                     method=params$rm.method, level=params$conf.level,
                     digits=params$digits)
    }
    #                        
    # Create list to display summary of results
    #
    degf <- res$k.yi - 1
    model.title <- paste("Binary Random-Effects Model (k = ", res$k, ")", sep="")
    data.type <- "binary"
    summary.disp <- create.summary.disp(res, params, degf, model.title, data.type)
 
    #
    # generate forest plot 
    #
    if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.binary(binary.data, params, res, types)
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
    }
    else {
        results <- list("Summary"=summary.disp)
    }    
    results
}


binary.random.parameters <- function(){
    # parameters
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary$MAResults
}