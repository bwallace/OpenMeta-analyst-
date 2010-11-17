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

# cd <- new('ContinuousData', N1=c(20, 90), mean1=c(1.2, .9), sd1=c(.2, .2), N2=c(30, 50), mean2=c(2, 2), sd2=c(.1, .1), study.names=c("1", "2"))
# params <- list(measure="MD", conf.level=95, digits=3)

compute.for.one.cont.study <- function(cont.data, params){
    res <- escalc(params$measure, 
                  n1i=cont.data@N1, m1i=cont.data@mean1, sd1i=cont.data@sd1,
                  n2i=cont.data@N2, m2i=cont.data@mean2, sd2i=cont.data@sd2)
                  
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
    res <- list("b"=c(y), "ci.lb"=lb, "ci.ub"=ub) 
    res
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
        Res <- list("rawResults" = res) 
        results <- list("Summary"=Res)
    }
    else{
        # otherwise, call out to the metafor package
        if (length(cont.data@mean1) > 0) {
            res<-rma.uni(n1i=cont.data@N1, n2i=cont.data@N2, 
                                        m1i=cont.data@mean1, m2i=cont.data@mean2,
                                        sd1i=cont.data@sd1, sd2i=cont.data@sd2,
                                        slab=cont.data@study.names,
                                        method=params$rm.method, measure=params$measure,
                                        level=params$conf.level, digits=params$digits)
        }
        else{
           res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                                        slab=cont.data@study.names,
                                        method=params$rm.method, level=params$conf.level,
                                        digits=params$digits)
        }
        degf <- res$k - res$p
        model.title <- paste("Continuous Random-Effects Model (k = ", res$k, ")", sep="")
        summary.disp <- create.summary.disp(res, params, degf, model.title)
        summary.disp
        
        #
        # generate forest plot 
        #
        if ((is.null(params$createPlot)) || (params$createPlot == TRUE)) {
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


continuous.random.parameters <- function(){
    # parameters
    cont_metrics <- c("MD", "SMD")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "measure"=cont_metrics, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "measure"="MD", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "measure", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

continuous.random.overall <- function(results){
    # this parses out the overall from the computed result
    res <- results$Summary$rawResults
    overall <- c(res$b[1], res$ci.lb, res$ci.ub)
    overall
}