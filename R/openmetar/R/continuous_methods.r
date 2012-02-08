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
    display.scale <- function(x){
        x
    }
    
    calc.scale <- function(x){
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
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{
        res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                     slab=cont.data@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- paste("Continuous Fixed-effect Model\n\nMetric: ", metric.name, sep="")
        data.type <- "cont"
        summary.disp <- create.summary.disp(res, params, model.title, data.type)
    }    
    #
    # generate forest plot 
    #
    if ((is.null(params$create.plot)) || (params$create.plot == TRUE)) {
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.continuous(cont.data, params, res)
        forest.plot(plot.data, outpath=forest.path)
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        
        plot.range <- plot.data$plot.range
            # plot range are values of params$fp_plot_lb and params$fp_plot_ub
            # if user has supplied them - otherwise they are calculated from data
            params$fp_plot_lb <- plot.range[1]
            params$fp_plot_ub <- plot.range[2]
        # dump the forest plot params to disk; return path to
        # this .Rdata for later use
 
        forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        results <- list("images"=images, "Summary"=summary.disp, 
                        "plot_names"=plot.names, "plot_params_paths"=plot.params.paths)
    }
    else {
        results <- list("Summary"=summary.disp)
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
    pretty.names <- list("pretty.name"="Continuous Fixed-effect Inverse Variance", 
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
    res <- results$Summary$MAResults
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
        summary.disp <- list("MAResults" = res) 
        results <- list("Summary"=summary.disp)
    }
    else{
        res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                     slab=cont.data@study.names,
                     method=params$rm.method, level=params$conf.level,
                     digits=params$digits)
        metric.name <- pretty.metric.name(as.character(params$measure))
        model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
        data.type <- "cont"
        summary.disp <- create.summary.disp(res, params, model.title, data.type)
    }        
    #
    # generate forest plot 
    #
    if ((is.null(params$create.plot)) || params$create.plot) {
        forest.path <- paste(params$fp_outpath, sep="")
        plot.data <- create.plot.data.continuous(cont.data, params, res)
        forest.plot(plot.data, outpath=forest.path)
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        plot.range <- plot.data$plot.range
        # plot range are values of params$fp_plot_lb and params$fp_plot_ub
        # if user has supplied them - otherwise they are calculated from data
        params$fp_plot_lb <- plot.range[1]
        params$fp_plot_ub <- plot.range[2]
        
        # dump the forest plot params to disk; return path to
        # this .Rdata for later use
        forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Forest Plot"=forest.path)
        plot.names <- c("forest plot"="forest_plot")
        results <- list("images"=images, "Summary"=summary.disp, 
                        "plot_names"=plot.names, "plot_params_paths"=plot.params.paths)
    }
    else {
        results <- list("Summary"=summary.disp)
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
    res <- results$Summary$MAResults
}