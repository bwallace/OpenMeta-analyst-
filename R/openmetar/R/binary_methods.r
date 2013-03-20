
####################################
# OpenMeta[Analyst]                #
# ----                             #
# binary_methods.r                 # 
# Facade module; wraps methods     #
# that perform analysis on binary  #
# data in a coherent interface.    # 
####################################

library(metafor)

binary.logit.metrics <- c("PLO")
binary.log.metrics <- c("OR", "RR", "PLN")
binary.arcsine.metrics <- c("PAS")
# The two-arm metric arcsine risk difference (AS) is not included in binary.arcsine.metrics
# so that display scale will be same as calculation scale.
binary.freeman_tukey.metrics <- c("PFT")
binary.two.arm.metrics <- c("OR", "RD", "RR", "AS", "YUQ", "YUY")
binary.one.arm.metrics <- c("PR", "PLN", "PLO", "PAS", "PFT")


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
    display.scale <- function(x, ...){
        if (metric.str %in% binary.log.metrics){
            exp(x)
        } else if (metric.str %in% binary.logit.metrics){
            invlogit(x)
        } else if (metric.str %in% binary.arcsine.metrics){
            invarcsine.sqrt(x)
        } else if (metric.str %in% binary.freeman_tukey.metrics){
              if (length(x)==1) {
                   # If x has length 1, use harmonic mean inverse transform, which takes the harmonic mean of n as second arg. 
                   # If n also has length 1, this is the same as trans.ipft(x,n).
                  transf.ipft.hm(x, targs=list(ni=n))
              } else {
                  transf.ipft(x, n)
              }
        } else {  
            # identity function
            x
        }
    }    

    
    calc.scale <- function(x, ...){
        if (metric.str %in% binary.log.metrics){
            log(x)
        } else if (metric.str %in% binary.logit.metrics){
            logit(x)   
        } else if (metric.str %in% binary.arcsine.metrics){
            arcsine.sqrt(x) 
        } else if (metric.str %in% binary.freeman_tukey.metrics){
          if (length(x)==1) {
             transf.pft(x, n)
          }
        } else {
            # identity function
            x
        }
    }

    list(display.scale = display.scale, calc.scale = calc.scale)
}

get.res.for.one.binary.study <- function(binary.data, params) {
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

create.binary.data.array <- function(binary.data, params, res){
    # Extracts data from binary.data and puts it into an array for the the first summary display table.
    tx1.name <- "tx A"
    tx2.name <- "tx B"
    # TODO: these should be taken from the corresponding column labels in the GUI and passed in via params.
    digits.str <- paste("%.", params$digits, "f", sep="")
    effect.size.name <- pretty.metric.name(as.character(params$measure))
    # Caculate confidence intervals
    study.ci.bounds <- calc.ci.bounds(binary.data, params)
    y.disp <- binary.transform.f(params$measure)$display.scale(binary.data@y)
    lb.disp <- binary.transform.f(params$measure)$display.scale(study.ci.bounds$lb)
    ub.disp <- binary.transform.f(params$measure)$display.scale(study.ci.bounds$ub)
    y <- sprintf(digits.str, y.disp)
    LL <- sprintf(digits.str, lb.disp)
    UL <- sprintf(digits.str, ub.disp)
    weights <- res$study.weights
    weights <- sprintf(digits.str, weights)
    weights <- format(weights, justify="right")
    # Extract the data from binary.data and round
    event.txA <- format(binary.data@g1O1, justify="right")
    subject.txA <- format(binary.data@g1O1 + binary.data@g1O2, justify="right")
    
    if (params$measure %in% binary.two.arm.metrics) {
        event.txB <- format(binary.data@g2O1, justify="right")
        subject.txB <- format(binary.data@g2O1 + binary.data@g2O2, justify="right")  
        raw.data <- array(c("Study", paste(binary.data@study.names, " ", binary.data@years, sep=""), 
                      paste(tx1.name, " Events", sep=""), event.txA, 
                      paste(tx1.name, " Subjects", sep=""), subject.txA, 
                      paste(tx2.name, " Events", sep=""), event.txB, 
                      paste(tx2.name, " Subjects", sep=""), subject.txB, 
                      effect.size.name, y, "Lower", LL, "Upper", UL, "Weight", weights), 
                      dim=c(length(binary.data@study.names) + 1, 9))
        class(raw.data) <- "summary.data" 
    } else if (params$measure %in% binary.one.arm.metrics) {
        raw.data <- array(c("Study", paste(binary.data@study.names, " ", binary.data@years, sep=""), 
                      paste(tx1.name, " Events", sep=""), event.txA, 
                      paste(tx1.name, " Subjects", sep=""), subject.txA, 
                      effect.size.name, y, "Lower", LL, "Upper", UL, "Weight", weights),
                      dim=c(length(binary.data@study.names) + 1, 7))
    }
    return(raw.data)
}

write.bin.study.data.to.file <- function(binary.data, params, res, data.outpath) {
    # create data frame and write to csv
    effect.size.name <- pretty.metric.name(as.character(params$measure))
    y.disp <- binary.transform.f(params$measure)$display.scale(binary.data@y)
    study.ci.bounds <- calc.ci.bounds(binary.data, params)
    if (params$measure %in% binary.two.arm.metrics) {
        study.data.df <- data.frame("study.names"=paste(binary.data@study.names, " ", binary.data@years, sep=""),
                            "txA.events" = binary.data@g1O1,
                            "txA.subjects" = binary.data@g1O1 + binary.data@g1O2,
                            "txB.events" = binary.data@g2O1,
                            "txB.subjects" = binary.data@g2O1 + binary.data@g2O2,
                            "Effect.size" = binary.transform.f(params$measure)$display.scale(binary.data@y),
                            "Lower.bound" = binary.transform.f(params$measure)$display.scale(study.ci.bounds$lb),
                            "Upper.bound" = binary.transform.f(params$measure)$display.scale(study.ci.bounds$ub),
                            "Weight" = res$study.weights)
    } else if(params$measure %in% binary.one.arm.metrics) {
        study.data.df <- data.frame("study.names"=paste(binary.data@study.names, " ", binary.data@years, sep=""),
                            "txA.events" = binary.data@g1O1,
                            "txA.subjects" = binary.data@g1O1 + binary.data@g1O2,
                            "Effect.size" = binary.transform.f(params$measure)$display.scale(binary.data@y),
                            "Lower.bound" = binary.transform.f(params$measure)$display.scale(study.ci.bounds$lb),
                            "Upper.bound" = binary.transform.f(params$measure)$display.scale(study.ci.bounds$ub),
                            "Weight" = res$study.weights)
    }
    # Rename effect size column
    names(study.data.df)[names(study.data.df)=="Effect.size"] <- effect.size.name
    write.csv(study.data.df, file=data.outpath, append=FALSE, row.names=FALSE)
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
        results <- list("Summary"=res)
    }
    else{
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
        if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
            if (is.null(params$write.to.file)) {
                # Write results and study data to csv files 
                # Weights assigned to each study
                res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
				## GD EXPERIMENTAL ##############
				res$study.names <- binary.data@study.names
				res$study.years <- binary.data@years
				##################################
                results.path <- "./r_tmp/binary_fixed_inv_var_results.csv"
                # @TODO Pass in results.path via params
                data.path <- "./r_tmp/binary_fixed_inv_var_study_data.csv"
                write.results.to.file(binary.data, params, res, outpath=results.path)
                # write.bin.study.data.to.file(binary.data, params, res, data.path)
                # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
            }
            if (is.null(params$create.plot)) {
                # Create forest plot and list to display summary of results
                metric.name <- pretty.metric.name(as.character(params$measure))
                model.title <- paste("Binary Fixed-Effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="")
                # Create results display tables
                summary.disp <- create.summary.disp(binary.data, params, res, model.title)
                forest.path <- paste(params$fp_outpath, sep="")
                plot.data <- create.plot.data.binary(binary.data, params, res)
                changed.params <- plot.data$changed.params
                # list of changed params values
                params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
                changed.params <- c(changed.params, params.changed.in.forest.plot)
                params[names(changed.params)] <- changed.params
                # update params values
                forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
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
                            "plot_names"=plot.names, 
                            "plot_params_paths"=plot.params.paths)
            }
        }
        else {
            results <- list("Summary"=res)
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

binary.fixed.inv.var.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Binary Fixed-Effect Inverse Variance", 
                         "description" = "Performs fixed-effect meta-analysis with inverse variance weighting.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                          )
}

binary.fixed.inv.var.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary
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
        results <- list("Summary"=res)
    }
    else{
        res<-rma.mh(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                ci=binary.data@g2O1, di=binary.data@g2O2, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, measure=params$measure,
                                add=c(params$adjust, 0), to=c(as.character(params$to), "none")) 
        if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
            if (is.null(binary.data@y) || is.null(binary.data@SE)) {
                # compute point estimates for plot.data in case they are missing
                binary.data <- compute.bin.point.estimates(binary.data, params)
            }
            if (is.null(params$write.to.file)) {
                # Write results and study data to csv files
                # Weights assigned to each study
                A <- binary.data@g1O1
                B <- binary.data@g1O2
                C <- binary.data@g2O1
                D <- binary.data@g2O2
                weights <- B * C / (A + B + C + D)
                res$study.weights <- weights / sum(weights)
				## GD EXPERIMENTAL ##############
				res$study.names <- binary.data@study.names
				res$study.years <- binary.data@years
				##################################
                results.path <- "./r_tmp/binary_fixed_mh_results.csv"
                # @TODO Pass in results.path via params
                data.path <- "./r_tmp/binary_fixed_mh_study_data.csv"
                write.results.to.file(binary.data, params, res, outpath=results.path)
                # write.bin.study.data.to.file(binary.data, params, res, data.outpath=data.path)
                # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
            }
            if (is.null(params$create.plot)) {
                # Create forest plot and list to display summary of results
                metric.name <- pretty.metric.name(as.character(params$measure))
                model.title <- paste("Binary Fixed-Effect Model - Mantel Haenszel\n\nMetric: ", metric.name, sep="")
                # Create results display tables
                summary.disp <- create.summary.disp(binary.data, params, res, model.title)
                forest.path <- paste(params$fp_outpath, sep="")
                plot.data <- create.plot.data.binary(binary.data, params, res)
                changed.params <- plot.data$changed.params
                # list of changed params values
                params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
                changed.params <- c(changed.params, params.changed.in.forest.plot)
                params[names(changed.params)] <- changed.params
                # dump the forest plot params to disk; return path to
                # this .Rdata for later use
                forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
                #
                # Now we package the results in a dictionary (technically, a named 
                # vector). In particular, there are two fields that must be returned; 
                # a dictionary of images (mapping titles to image paths) and a list of texts
                # (mapping titles to pretty-printed text). In this case we have only one 
                # of each. 
                # 
                references <- "Mantel, N., & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. Journal of the National Cancer Institute, 22, 719-748."
                plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
                images <- c("Forest Plot"=forest.path)
                plot.names <- c("forest plot"="forest_plot")
                results <- list("images"=images, "Summary"=summary.disp, "References"=references, 
                            "plot_names"=plot.names, "plot_params_paths"=plot.params.paths)
            }
        }
        else {
            results <- list("Summary"=res)
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

binary.fixed.mh.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Binary Fixed-Effect Mantel Haenszel", 
                         "description" = "Performs fixed-effect meta-analysis using the Mantel Haenszel method.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                          )
}


binary.fixed.mh.is.feasible <- function(binary.data, metric){
    # only feasible if we have raw (2x2) data for all studies
    # in this case the metric is ignored
    length(binary.data@g1O1)==length(binary.data@g1O2) &&
    length(binary.data@g1O2)==length(binary.data@g2O1) &&
    length(binary.data@g2O1)==length(binary.data@g2O2) &&
         length(binary.data@g1O1) > 0
}

binary.fixed.mh.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary
}
                                                                                                                         
##################################################
#       binary fixed effects -- Peto             #
##################################################
binary.fixed.peto <- function(binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.") 
    
    if (length(binary.data@g1O1) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        results <- list("Summary"=res)
    }
    else{  
        res <- rma.peto(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                                ci=binary.data@g2O1, di=binary.data@g2O2, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits)
        # Corrected values for y and SE
        binary.data@y <- res$yi
        binary.data@SE <- sqrt(res$vi)
        
        if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
            if (is.null(binary.data@y) || is.null(binary.data@SE)) {
                # compute point estimates for plot.data in case they are missing
                binary.data <- compute.bin.point.estimates(binary.data, params)
            }
            if (is.null(params$write.to.file)) {
                # Write results and study data to csv files  
                res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
				## GD EXPERIMENTAL ##############
				res$study.names <- binary.data@study.names
				res$study.years <- binary.data@years
				##################################
                results.path <- paste("./r_tmp/binary_fixed_peto_results.csv")
                # @TODO Pass in results.path via params
                data.path <- paste("./r_tmp/binary_fixed_peto_study_data.csv")
                write.results.to.file(binary.data, params, res, outpath=results.path)
                # write.bin.study.data.to.file(binary.data, params, res, data.outpath=data.path)
                # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
            }
            if (is.null(params$create.plot)) {
                # Create forest plot and list to display summary of results
                metric.name <- pretty.metric.name(as.character(params$measure))
                model.title <- "Binary Fixed-Effect Model - Peto\n\nMetric: Odds Ratio"
                # Create results display tables
                summary.disp <- create.summary.disp(binary.data, params, res, model.title)
                #
                # generate forest plot 
                #
                forest.path <- paste(params$fp_outpath, sep="")
                plot.data <- create.plot.data.binary(binary.data, params, res)
                changed.params <- plot.data$changed.params
                # list of changed params values
                params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
                changed.params <- c(changed.params, params.changed.in.forest.plot)
                params[names(changed.params)] <- changed.params
                # dump the forest plot params to disk; return path to
                # this .Rdata for later use
                forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
                #
                # Now we package the results in a dictionary (technically, a named 
                # vector). In particular, there are two fields that must be returned; 
                # a dictionary of images (mapping titles to image paths) and a list of texts
                # (mapping titles to pretty-printed text). In this case we have only one 
                # of each. 
                #
                references <- "Yusuf, S., Peto, R., Lewis, J., Collins, R., & Sleight, P. (1985). Beta blockade during and after myocardial infarction: An overview of the randomized trials. Progress in Cardiovascular Disease, 27, 335-371."
                plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
                images <- c("Forest Plot"=forest.path)
                plot.names <- c("forest plot"="forest_plot")
                results <- list("images"=images, "Summary"=summary.disp, "References"=references,
                            "plot_names"=plot.names, "plot_params_paths"=plot.params.paths)
            }
        }
        else {
            results <- list("Summary"=res)
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

binary.fixed.peto.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Binary Fixed-Effect Peto", 
                         "description" = "Performs fixed-effect meta-analysis using the Peto method.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                         )
}

binary.fixed.peto.is.feasible <- function(binary.data, metric){
    # only feasible if we have raw (2x2) data for all studies
    # and the metric is `OR'
    metric == "OR" &&
    length(binary.data@g1O1)==length(binary.data@g1O2) &&
    length(binary.data@g1O2)==length(binary.data@g2O1) &&
    length(binary.data@g2O1)==length(binary.data@g2O2) &&
         length(binary.data@g1O1) > 0
}

binary.fixed.peto.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary
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
        results <- list("Summary"=res)
    }
    else{     
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, 
                     slab=binary.data@study.names,
                     method=params$rm.method, level=params$conf.level,
                     digits=params$digits)
        if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
            if (is.null(binary.data@y) || is.null(binary.data@SE)) {
                # compute point estimates for plot.data in case they are missing
                binary.data <- compute.bin.point.estimates(binary.data, params)
            }
            if (is.null(params$write.to.file)) {
                # Write results and study data to csv files
                # Weights assigned to each study
                weights <- 1 / (res$vi + res$tau2)
                res$study.weights <- weights / sum(weights)
				
				## GD EXPERIMENTAL ##############
				res$study.names <- binary.data@study.names
				res$study.years <- binary.data@years
				##################################
				
                # Write results and study data to csv files
                results.path <- paste("./r_tmp/binary_random_results.csv")
                # @TODO Pass in results.path via params
                data.path <- paste("./r_tmp/binary_random_study_data.csv")
                write.results.to.file(binary.data, params, res, outpath=results.path)
                # write.bin.study.data.to.file(binary.data, params, res, data.outpath=data.path)
                # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
				###############################
				print("THE WEIGHTS:")         #
				print(res$study.weights)      #
				###############################
            }
            if (is.null(params$create.plot)) {
                # Create forest plot and list to display summary of results
                #
                metric.name <- pretty.metric.name(as.character(params$measure))
                model.title <- paste("Binary Random-Effects Model\n\nMetric: ", metric.name, sep="")
				
				###############################
				#print("RES HERE:")            #
				#print(res)                    #
				#
				#print("Res weights here")
				#print(res$study.weights)
				#
				#print("END OF RES HERE")
				###############################
				
                # Create results display tables
                summary.disp <- create.summary.disp(binary.data, params, res, model.title)
                #
                # generate forest plot 
                #
                forest.path <- paste(params$fp_outpath, sep="")
                plot.data <- create.plot.data.binary(binary.data, params, res)
                changed.params <- plot.data$changed.params
                # list of changed params values
                params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
                changed.params <- c(changed.params, params.changed.in.forest.plot)
                params[names(changed.params)] <- changed.params
                # dump the forest plot params to disk; return path to
                # this .Rdata for later use
                forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
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


binary.random.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float",
                   "adjust"="float", "to"=apply_adjustment_to)
       
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order <- c("rm.method", "conf.level", "digits", "adjust", "to")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.random.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Binary Random-Effects", 
                         "description" = "Performs random-effects meta-analysis.",
                         "rm.method"=list("pretty.name"="Random method", "description"="Method for estimating between-studies heterogeneity"),                      
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                         )
}

binary.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$Summary
}