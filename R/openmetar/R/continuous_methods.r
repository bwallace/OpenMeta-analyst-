####################################
# OpenMeta[Analyst]                #
# ----                             #
# continuous_methods.r             # 
# Facade module; wraps methods     #
# that perform analyses on         # 
# continuous data in a coherent    #
# interface.                       #
####################################

library(metafor)

continuous.two.arm.metrics <- c("MD", "SMD")
continuous.one.arm.metrics <- c("TXMean")

compute.for.one.cont.study <- function(cont.data, params){
    n1i <- cont.data@N1
    n2i <- cont.data@N2
    m1i <- cont.data@mean1
    m2i <- cont.data@mean2
    sd1i <- cont.data@sd1
    sd2i <- cont.data@sd2
    res <- escalc(params$measure, n1i=n1i, n2i=n2i, m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i)
    res
}

continuous.transform.f <- function(metric.str){
    display.scale <- function(x, ...){
        x
    }
    
    calc.scale <- function(x, ...){
        x
    }
    
    list(display.scale = display.scale, calc.scale = calc.scale)
}

get.res.for.one.cont.study <- function(cont.data, params){
    # this method can be called when there is only one study to 
    # get the point estimate and lower/upper bounds.
    y<-NULL
    se<-NULL
    if (length(cont.data@y) == 0 || is.na(cont.data@y)){
        res <- compute.for.one.cont.study(cont.data, params)    
        y <- res$yi[1]
        se <- sqrt(res$vi[1])
    }
    else{
        y <- cont.data@y[1]
        se <- cont.data@SE[1]
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

create.cont.data.array <- function(cont.data, params, res){
  # Extracts data from cont.data and puts it into an array for the the first summary display table.
  tx1.name <- "tx A"
  tx2.name <- "tx B"
  # TODO: these should be taken from the corresponding column labels in the GUI and passed in via params.
  digits.str <- paste("%.", params$digits, "f", sep="")
  effect.size.name <- pretty.metric.name(as.character(params$measure))
  # Caculate confidence intervals
  study.ci.bounds <- calc.ci.bounds(cont.data, params)
  y.disp <- continuous.transform.f(params$measure)$display.scale(cont.data@y)
  lb.disp <- continuous.transform.f(params$measure)$display.scale(study.ci.bounds$lb)
  ub.disp <- continuous.transform.f(params$measure)$display.scale(study.ci.bounds$ub)
  y <- sprintf(digits.str, y.disp)
  LL <- sprintf(digits.str, lb.disp)
  UL <- sprintf(digits.str, ub.disp)
  weights <- res$study.weights
  weights <- sprintf(digits.str, weights)
  weights <- format(weights, justify="right")
  # Extract the data from cont.data and round
  N.txA <- format(cont.data@N1, justify="right")
  mean.txA <- sprintf(digits.str, cont.data@mean1)
  sd.txA <- sprintf(digits.str, cont.data@sd1)
  if (params$measure %in% continuous.two.arm.metrics) {
      N.txB <- format(cont.data@N2, justify="right")
      mean.txB <- sprintf(digits.str, cont.data@mean2)
      sd.txB <- sprintf(digits.str, cont.data@sd2)
      raw.data <- array(c("Study", cont.data@study.names, 
                    paste(tx1.name, " N", sep=""), N.txA, 
                    paste(tx1.name, " Mean", sep=""), mean.txA, 
                    paste(tx1.name, " SD", sep=""), sd.txA, 
                    paste(tx2.name, " N", sep=""), N.txB,
                    paste(tx2.name, " Mean", sep=""), mean.txB, 
                    paste(tx2.name, " SD", sep=""), sd.txB,
                                                      effect.size.name, y, "Lower", LL, "Upper", UL, "Weight", weights), 
                    dim=c(length(cont.data@study.names) + 1, 11))
  } else if (params$measure %in% continuous.one.arm.metrics) {
      raw.data <- array(c("Study", cont.data@study.names, 
                    paste(tx1.name, " N", sep=""), N.txA, 
                    paste(tx1.name, " Mean", sep=""), mean.txA, 
                    paste(tx1.name, " SD", sep=""), sd.txA,
                    effect.size.name, y, "Lower", LL, "Upper", UL, "Weight", weights), 
                    dim=c(length(cont.data@study.names) + 1, 8))
  }
  class(raw.data) <- "summary.data" 
  return(raw.data)
}

write.cont.study.data.to.file <- function(cont.data, params, res, data.outpath) {
  # create data frame and write to csv
  effect.size.name <- pretty.metric.name(as.character(params$measure))
  # Caculate confidence intervals
  study.ci.bounds <- calc.ci.bounds(cont.data, params)
  y.disp <- continuous.transform.f(params$measure)$display.scale(cont.data@y)
  if (params$measure %in% continuous.two.arm.metrics) {
      study.data.df <- data.frame("study.names"=paste(cont.data@study.names, " ", cont.data@years, sep=""),
                                "N1" = cont.data@N1,
                                "mean1" = cont.data@mean1,
                                "sd1" = cont.data@sd1,
                                "N2" = cont.data@N2,
                                "mean2" = cont.data@mean2,
                                "sd2" = cont.data@sd1,
                                "Effect.size" = continuous.transform.f(params$measure)$display.scale(cont.data@y),
                                "Lower.bound" = continuous.transform.f(params$measure)$display.scale(study.ci.bounds$lb),
                                "Upper.bound" = continuous.transform.f(params$measure)$display.scale(study.ci.bounds$ub),
                                "Weight" = res$study.weights)
  } else if(params$measure %in% continuous.one.arm.metrics) {
    study.data.df <- data.frame("study.names"=paste(cont.data@study.names, " ", cont.data@years, sep=""),
                                "N1" = cont.data@N1,
                                "mean1" = cont.data@mean1,
                                "sd1" = cont.data@sd1,
                                "Effect.size" = continuous.transform.f(params$measure)$display.scale(cont.data@y),
                                "Lower.bound" = continuous.transform.f(params$measure)$display.scale(study.ci.bounds$lb),
                                "Upper.bound" = continuous.transform.f(params$measure)$display.scale(study.ci.bounds$ub),
                                "Weight" = res$study.weights)
  }
  # Rename effect size column
  names(study.data.df)[names(study.data.df)=="Effect.size"] <- effect.size.name
  write.csv(study.data.df, file=data.outpath, append=FALSE, row.names=FALSE)
}

###############################
#  continuous fixed effects  #
###############################
continuous.fixed <- function(cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    results <- NULL
    if (length(cont.data@study.names) == 1){
        # handle the case where only one study was passed in
        res <- get.res.for.one.cont.study(cont.data, params)   
         # Package res for use by overall method.
        results <- list("Summary"=res)
    }
    else {
        res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                     slab=cont.data@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
        if (is.null(params$create.plot) || (is.null(params$write.to.file))) {
            if (is.null(params$write.to.file)) {
                # Write results and study data to csv files
                # Weights assigned to each study
                res$study.weights <- (1 / res$vi) / sum(1 / res$vi)
				## GD EXPERIMENTAL ##############
				res$study.names <- cont.data@study.names
				res$study.years <- cont.data@years
				##################################
                results.path <- paste("./r_tmp/cont_fixed_results.csv")
                # @TODO Pass in results.path via params
                data.path <- paste("./r_tmp/cont_fixed_study_data.csv")
                write.results.to.file(cont.data, params, res, outpath=results.path)
                # write.cont.study.data.to.file(cont.data, params, res, data.outpath=data.path)
                # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
            }
            if (is.null(params$create.plot)) {
                # Create forest plot and list to display summary of results
                metric.name <- pretty.metric.name(as.character(params$measure))
                model.title <- paste("Continuous Fixed-Effect Model\n\nMetric: ", metric.name, sep="")
                summary.disp <- create.summary.disp(cont.data, params, res, model.title)
                #
                # generate forest plot 
                #
                forest.path <- paste(params$fp_outpath, sep="")
                plot.data <- create.plot.data.continuous(cont.data, params, res)
                changed.params <- plot.data$changed.params
                # list of changed params values
                params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
                changed.params <- c(changed.params, params.changed.in.forest.plot)
                params[names(changed.params)] <- changed.params
                # dump the forest plot params to disk; return path to
                # this .Rdata for later use
                forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
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

continuous.fixed.parameters <- function(){
    # parameters
    params <- list("conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3)
    
    var_order <- c("conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

continuous.fixed.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Continuous Fixed-Effect Inverse Variance", 
                         "description" = "Performs fixed-effect meta-analysis with inverse variance weighting.",
                         "conf.level"=list("pretty.name"="Confidence level", "description"="Level at which to compute confidence intervals"), 
                         "digits"=list("pretty.name"="Number of digits", "description"="Number of digits to display in results"),
                         "adjust"=list("pretty.name"="Correction factor", "description"="Constant c that is added to the entries of a two-by-two table."),
                         "to"=list("pretty.name"="Add correction factor to", "description"="When Add correction factor is set to \"only 0\", the correction factor
                                   is added to all cells of each two-by-two table that contains at leason one zero. When set to \"all\", the correction factor
                                   is added to all two-by-two tables if at least one table contains a zero.")
                          )
}

continuous.fixed.overall <- function(results){
    # this parses out the overall from the computed result
    res <- results$Summary
}

###############################
#  continuous random effects  #
###############################
continuous.random <- function(cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    results <- NULL
    if (length(cont.data@study.names) == 1){
        # handle the case where only one study was passed in
        res <- get.res.for.one.cont.study(cont.data, params)   
         # Package res for use by overall method.
        results <- list("Summary"=res)
    }
    else{
        res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                     slab=cont.data@study.names,
                     method=params$rm.method, level=params$conf.level,
                     digits=params$digits)

        
        results <- list("Summary"=res)

        ###
        # @TODO this needs major re-factoring -- totally
        #   unreadable / illogical
        if (is.null(params$create.plot) || (is.null(params$write.to.file)) || params$create.plot || params$write.to.file) {
          if (is.null(params$write.to.file) || params$write.to.file) {
              # Write results and study data to csv files
              # Weights assigned to each study
              weights <- 1 / (res$vi + res$tau2)
              res$study.weights <- weights / sum(weights)
			  
			  ## GD EXPERIMENTAL ##############
			  res$study.names <- cont.data@study.names
			  res$study.years <- cont.data@years
			  ##################################
			  
              results.path <- "./r_tmp/cont_random_results.csv"
              # @TODO Pass in results.path via params
              data.path <- "./r_tmp/cont_random_study_data.csv"
              write.results.to.file(cont.data, params, res, outpath=results.path)
              # write.cont.study.data.to.file(cont.data, params, res, data.outpath=data.path)
              # @TODO: Check for non-numeric entries and replace with blanks to avoid errors.
          }

          if (is.null(params$create.plot) || params$create.plot) {
              # Create forest plot and list to display summary of results
              metric.name <- pretty.metric.name(as.character(params$measure))
              model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
              summary.disp <- create.summary.disp(cont.data, params, res, model.title)
              #
              # generate forest plot 
              #
              forest.path <- paste(params$fp_outpath, sep="")
              plot.data <- create.plot.data.continuous(cont.data, params, res)
              changed.params <- plot.data$changed.params
              # list of changed params values
              params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
              changed.params <- c(changed.params, params.changed.in.forest.plot)
              params[names(changed.params)] <- changed.params
              # dump the forest plot params to disk; return path to
              # this .Rdata for later use
              forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
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
    }
    results
}


continuous.random.parameters <- function(){
    # parameters
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

continuous.random.pretty.names <- function() {
    pretty.names <- list("pretty.name"="Continuous Random-Effects", 
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

continuous.random.overall <- function(results){
    # this parses out the overall from the computed result
    res <- results$Summary
}