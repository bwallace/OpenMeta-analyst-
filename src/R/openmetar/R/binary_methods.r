
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
        
        extra.args <- list(...)
        
        if (metric.str %in% binary.log.metrics){
            exp(x)
        } else if (metric.str %in% binary.logit.metrics){
            invlogit(x)
        } else if (metric.str %in% binary.arcsine.metrics){
            invarcsine.sqrt(x)
        } else if (metric.str %in% binary.freeman_tukey.metrics){
              ni <- extra.args[['ni']]
              if (length(x)==1) {
                   # If x has length 1, use harmonic mean inverse transform, which takes the harmonic mean of n as second arg. 
                   # If n also has length 1, this is the same as trans.ipft(x,n).
                  transf.ipft.hm(x, targs=list(ni=ni))
              } else {
                  transf.ipft(x, ni)
              }
        } else {  
            # identity function
            x
        }
    }    

    
    calc.scale <- function(x, ...){
        
        extra.args <- list(...)
        if (metric.str %in% binary.log.metrics){
            log(x)
        } else if (metric.str %in% binary.logit.metrics){
            logit(x)   
        } else if (metric.str %in% binary.arcsine.metrics){
            arcsine.sqrt(x) 
        } else if (metric.str %in% binary.freeman_tukey.metrics){
            ni <- extra.args[['ni']]
          if (length(x)==1) {
             transf.pft(x, ni)
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

#convert.res.conf.level(data) {
##    ;
##}



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
    if (!("BinaryData" %in% class(binary.data)))
        stop("Binary data expected.")
    
    results <- NULL
    input.params <- params
    
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
        # Package res for use by overall method.
        results <- list("Summary"=res,
                        "res"=res)
    } else {
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=c(params$adjust,params$adjust),
                                to=c(as.character(params$to), as.character(params$to)))
        pure.res <- res
        # Create forest plot and list to display summary of results
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- paste("Binary Fixed-Effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="")
        # Create results display tables
        summary.disp <- create.summary.disp(binary.data, params, res, model.title)
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.binary(binary.data, params, res)
        changed.params <- plot.data$changed.params
        # list of changed params values
		
		
		forest.plot.params.path <- ""
		if (is.null(params$supress.output) || !params$supress.output) {
            params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
            changed.params <- c(changed.params, params.changed.in.forest.plot)
            params[names(changed.params)] <- changed.params
            # update params values
            forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
		}
        
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #  
        
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        pure.res$weights <- weights(res)
        results <- list("input_data"=binary.data,
                        "input_params"=input.params,
                        "images"=images,
                        "Summary"=capture.output.and.collapse(summary.disp),
                        "plot_names"=plot.names, 
                        "plot_params_paths"=plot.params.paths,
                        "res"=pure.res,
                        "res.info"=binary.fixed.inv.var.value.info(),
                        "weights"=weights(res))
    }
    
    references <- "this is a placeholder for binary fixed effect inv var reference"
    results[["References"]] <- references
    results
}

binary.fixed.inv.var.value.info <- function() {
    rma.uni.value.info()
}

binary.fixed.inv.var.is.feasible.for.funnel <- function() {
    TRUE
}
                                
binary.fixed.inv.var.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list("conf.level"="float",
                   "digits"="int", 
                   "adjust"="float",
                   "to"=apply_adjustment_to)
    
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
    res <- results$res
}

############################################
#  binary fixed effects -- mantel haenszel #
############################################
binary.fixed.mh <- function(binary.data, params){    
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data)))
        stop("Binary data expected.")  
    
    results <- NULL
    input.params <- params
    
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        results <- list("Summary"=res, # shouldn't assume this is any more than a string
                        "res"=res) # actual metafor output
    } else {
        res<-rma.mh(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                    ci=binary.data@g2O1, di=binary.data@g2O2,
                    slab=binary.data@study.names,
                    level=params$conf.level,
                    digits=params$digits,
                    measure=params$measure,
                    add=c(params$adjust, 0),
                    to=c(as.character(params$to), "none"))
        pure.res <- res
        if (is.null(binary.data@y) || is.null(binary.data@SE)) {
            # compute point estimates for plot.data in case they are missing
            binary.data <- compute.bin.point.estimates(binary.data, params)
        }
        # Create forest plot and list to display summary of results
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- paste("Binary Fixed-Effect Model - Mantel Haenszel\n\nMetric: ", metric.name, sep="")
        # Create results display tables
        summary.disp <- create.summary.disp(binary.data, params, res, model.title)
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.binary(binary.data, params, res)
        changed.params <- plot.data$changed.params
		
		forest.plot.params.path <- ""
		if (is.null(params$supress.output) || !params$supress.output) {
	        # list of changed params values
	        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
	        changed.params <- c(changed.params, params.changed.in.forest.plot)
	        params[names(changed.params)] <- changed.params
	        # dump the forest plot params to disk; return path to
	        # this .Rdata for later use
	        forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
		}
        
		
		
		
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        # 
        #references <- "Mantel, N., & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. Journal of the National Cancer Institute, 22, 719-748."
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        pure.res$weights <- weights(res)
        results <- list("input_data"=binary.data,
                        "input_params"=input.params,
                        "images"=images,
                        "Summary"=capture.output.and.collapse(summary.disp), 
                        "plot_names"=plot.names,
                        "plot_params_paths"=plot.params.paths,
                        "res"=pure.res,
                        "res.info"=binary.fixed.mh.value.info(),
                        "weights"=weights(res))
            
           
    }
    
    references <- "Mantel, N., & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. Journal of the National Cancer Institute, 22, 719-748."
    results[["References"]] = references
    
    results
}


binary.fixed.mh.value.info <- function() {
    list(
        b        = list(type="vector", description='estimated coefficients of the model.'),
        se       = list(type="vector", description='standard errors of the coefficients.'),
        zval     = list(type="vector", description='test statistics of the coefficients.'),
        pval     = list(type="vector", description='p-values for the test statistics.'),
        ci.lb    = list(type="vector", description='lower bound of the confidence intervals for the coefficients.'),
        ci.ub    = list(type="vector", description='upper bound of the confidence intervals for the coefficients.'),
        QE       = list(type="vector", description='test statistic for the test of (residual) heterogeneity.'),
        QEp      = list(type="vector", description='p-value for the test of (residual) heterogeneity.'),
        MH       = list(type="vector", description='Cochran-Mantel-Haenszel test statistic (measure="OR") or Mantel-Haenszel test statistic (measure="IRR").'),
        MHp      = list(type="vector", description='corresponding p-value'),
        TA       = list(type="vector", description='Tarone’s heterogeneity test statistic (only when measure="OR").'), 
        TAp      = list(type="vector", description='corresponding p-value (only when measure="OR").'), 
        k        = list(type="vector", description='number of tables included in the analysis.'), 
        yi       = list(type="vector", description='the vector of outcomes'),
        vi       = list(type="vector", description='the corresponding sample variances'),
        fit.stats= list(type="data.frame", description='a list with the log-likelihood, deviance, AIC, BIC, and AICc values under the unrestricted and restricted likelihood.'),
    
        # not part of rma.mh default output
        weights = list(type="vector", description="weights in % given to the observed effects")
)
}
                                
binary.fixed.mh.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list("conf.level"="float", "digits"="int",
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

binary.fixed.mh.is.feasible.for.funnel <- function() {
    FALSE
}

binary.fixed.mh.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$res
}
                                                                                                                         
##################################################
#       binary fixed effects -- Peto             #
##################################################
binary.fixed.peto <- function(binary.data, params) {    
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data)))
        stop("Binary data expected.") 
    
    input.params <- params
    
    if (length(binary.data@g1O1) == 1) {
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        results <- list("Summary"=res,
                        "res"=res)
    } else {
           res <- rma.peto(ai=binary.data@g1O1, bi=binary.data@g1O2, 
                        ci=binary.data@g2O1, di=binary.data@g2O2,
                        slab=binary.data@study.names,
                        level=params$conf.level,
                        digits=params$digits,
                        add=c(params$adjust,params$adjust),
                        to=c(as.character(params$to), as.character(params$to)),
                        drop00 = FALSE)  # needed in metafor 1.8, unknown in 1.6
        pure.res <- res
        # Corrected values for y and SE
        binary.data@y <- res$yi
        binary.data@SE <- sqrt(res$vi)
        
        if (is.null(binary.data@y) || is.null(binary.data@SE)) {
            # compute point estimates for plot.data in case they are missing
            binary.data <- compute.bin.point.estimates(binary.data, params)
        }
        
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
		
		forest.plot.params.path <- ""
		if (is.null(params$supress.output) || !params$supress.output) {
	        # list of changed params values
	        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
	        changed.params <- c(changed.params, params.changed.in.forest.plot)
	        params[names(changed.params)] <- changed.params
	        # dump the forest plot params to disk; return path to
	        # this .Rdata for later use
	        forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
		}
        
		
		
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        pure.res$weights <- weights(res)
        results <- list("input_data"=binary.data,
                        "input_params"=input.params,
                        "images"=images,
                        "Summary"=capture.output.and.collapse(summary.disp),
                        "plot_names"=plot.names,
                        "plot_params_paths"=plot.params.paths,
                        "res"=pure.res, # if res is here, res.info must be too
                        "res.info"=binary.fixed.peto.value.info(),
                        "weights"=weights(res))
    }
    
    references <- "Fixed Peto: Yusuf, S., Peto, R., Lewis, J., Collins, R., & Sleight, P. (1985). Beta blockade during and after myocardial infarction: An overview of the randomized trials. Progress in Cardiovascular Disease, 27, 335-371."
    results[["References"]] <- references
    results
}

binary.fixed.peto.value.info <- function() {
    list(
            b        = list(type="vector", description='estimated coefficients of the model.'),
            se       = list(type="vector", description='standard errors of the coefficients.'),
            zval     = list(type="vector", description='test statistics of the coefficients.'),
            pval     = list(type="vector", description='p-values for the test statistics.'),
            ci.lb    = list(type="vector", description='lower bound of the confidence intervals for the coefficients.'),
            ci.ub    = list(type="vector", description='upper bound of the confidence intervals for the coefficients.'),
            QE       = list(type="vector", description='test statistic for the test of heterogeneity.'),
            QEp      = list(type="vector", description='p-value for the test of heterogeneity.'),
            k        = list(type="vector", description='number of tables included in the analysis'),
            yi       = list(type="vector", description='the vector of outcomes'),
            vi       = list(type="vector", description='the corresponding sample variances'),
            fit.stats= list(type="data.frame", description='a list with the log-likelihood, deviance, AIC, BIC, and AICc values under the unrestricted and restricted likelihood.'),
            
            # not part of rma.peto output
            weights = list(type="vector", description="weights in % given to the observed effects")
    )
}
                              
binary.fixed.peto.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list("conf.level"="float", "digits"="int",
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

binary.fixed.peto.is.feasible.for.funnel <- function() {
    FALSE
}

binary.fixed.peto.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$res
}


##################################
#  binary random effects         #
##################################
binary.random <- function(binary.data, params) {    
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    
    results <- NULL
    input.params <- params
    
    if (length(binary.data@g1O1) == 1 || length(binary.data@y) == 1){
        res <- get.res.for.one.binary.study(binary.data, params)
         # Package res for use by overall method.
        results <- list("Summary"=res,
                        "res"=res)
    } else {     
        # call out to the metafor package
        res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, 
                     slab=binary.data@study.names,
                     method=params$rm.method, level=params$conf.level,
                     digits=params$digits,
                     add=c(params$adjust,params$adjust),
                     to=as.character(params$to))
                     ##drop00 = FALSE)  # needed in metafor 1.8, unknown in 1.6
        pure.res <- res # store res before it gets messed with
        if (is.null(binary.data@y) || is.null(binary.data@SE)) {
            # compute point estimates for plot.data in case they are missing
            binary.data <- compute.bin.point.estimates(binary.data, params)
        }
        # Create forest plot and list to display summary of results
        #
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- paste("Binary Random-Effects Model\n\nMetric: ", metric.name, sep="")
        
        # Create results display tables
        summary.disp <- create.summary.disp(binary.data, params, res, model.title)
        #
        # generate forest plot 
        
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.binary(binary.data, params, res)
        changed.params <- plot.data$changed.params
		
		forest.plot.params.path <- ""
		if (is.null(params$supress.output) || !params$supress.output) {
	        # list of changed params values
	        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
	        changed.params <- c(changed.params, params.changed.in.forest.plot)
	        params[names(changed.params)] <- changed.params
	        # dump the forest plot params to disk; return path to
	        # this .Rdata for later use
	        forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
		}
        
		
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #        
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        pure.res$weights <- weights(res) # not pure anymore, oh well
        results <- list("input_data"=binary.data, # the data that was given to the routine in the first place
                        "input_params"=input.params,
                        "images"=images,
                        "Summary"=capture.output.and.collapse(summary.disp),
                        "plot_names"=plot.names,
                        "plot_params_paths"=plot.params.paths,
                        "res"=pure.res, # the results directly from metafor in order to extract values of interests
                        "res.info"=binary.random.value.info(),
                        "weights"=weights(res))
    }
    
    references <- "this is a placeholder for binary random reference"
    results[["References"]] <- references
    results
}

# Returns list mapping name-->type for the pure results output by metafor
binary.random.value.info <- function() {
    rma.uni.value.info()
}

binary.random.is.feasible.for.funnel <- function () {
    TRUE
}


binary.random.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="int",
                   "adjust"="float", "to"=apply_adjustment_to)
       
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order <- c("rm.method", "conf.level", "digits", "adjust", "to")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.random.pretty.names <- function() {
    # sort of redundant to have both this and rm_method_ls but whatever for now...
    rm_method_names <- list(
            HE="Hedges-Olkin",
            DL = "DerSimonian-Laird",
            SJ = "Sidik-Jonkman",
            ML = "Maximum Likelihood",
            REML = "Restricted Maximum Likelihood", 
            EB = "Empirical Bayes")
    
    pretty.names <- list("pretty.name"="Binary Random-Effects", 
                         "description" = "Performs random-effects meta-analysis.",
                         "rm.method"=list("pretty.name"="Random-Effects method", "description"="Method for estimating between-studies heterogeneity", "rm.method.names"=rm_method_names),                      
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits of precision to display", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Cells to which correction factor should be added", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                         )
}

binary.random.overall <- function(results) {
    # this parses out the overall from the computed result
    res <- results$res
}