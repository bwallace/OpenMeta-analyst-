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

# cd <- new('ContinuousData', N1=c(20, 90), mean1=c(1.2, .9), sd1=c(.2, .2), N2=c(30, 50), mean2=c(2, 2), sd2=c(.1, .1), studyNames=c("1", "2"))
# params <- list(measure="MD", conf.level=95, digits=3)

compute.for.one.cont.study <- function(contData, params){
    res <- escalc(params$metric, 
                  n1i=contData@N1, m1i=contData@mean1, sd1i=contData@sd1,
                  n2i=contData@N2, m2i=contData@mean2, sd2i=contData@sd2)
                  
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

get.res.for.one.cont.study <- function(contData, params){
    # this method can be called when there is only one study to 
    # get the point estimate and lower/upper bounds.
    y<-NULL
    se<-NULL
    if (length(contData@y) == 0 || is.na(contData@y)){
        res <- compute.for.one.cont.study(contData, params)    
        y <- res$yi[1]
        se <- sqrt(res$vi[1])
    }
    else{
        y <- contData@y[1]
        se <- contData@SE[1]
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
continuous.random <- function(contData, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(contData))) stop("Continuous data expected.")
    
    results <- NULL
    if (length(contData@studyNames) == 1){
        # handle the case where only one study was passed in
        res <- get.res.for.one.cont.study(contData, params)   
        results <- list("summary"=res)
    }
    else{
        # otherwise, call out to the metafor package
        if (length(contData@mean1) > 0) {
            res<-rma.uni(n1i=contData@N1, n2i=contData@N2, 
                                        m1i=contData@mean1, m2i=contData@mean2,
                                        sd1i=contData@sd1, sd2i=contData@sd2,
                                        slab=contData@studyNames,
                                        method=params$rm.method, measure=params$measure,
                                        level=params$conf.level, digits=params$digits)
        }
        else{
           res<-rma.uni(yi=contData@y, sei=contData@SE, 
                                        slab=contData@studyNames,
                                        method=params$rm.method, level=params$conf.level,
                                        digits=params$digits)
        }
        degf <- res$k - res$p
        modelTitle <- paste("Continuous Random-Effects Model (k = ", res$k, ")", sep="")
        summaryDisp <- createSummaryDisp(res, params, degf, modelTitle)
        #summaryDisp$modelTitle <- paste("Random-Effects Model - Inverse Variance (k = ", res$k, ")", sep="")
        summaryDisp
        #
        # generate forest plot 
        #
        getwd()
        forest.path <- "./r_tmp/forest.png"
        #png(forest_path)
        plotData <- create.plot.data.continuous(contData, params, res)
        forest.plot(plotData, outpath=forest.path)
    
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        images <- c("forest plot"=forest.path)
        plot_names <- c("forest plot"="forest_plot")
        
        results <- list("images"=images, "Summary"=summaryDisp, "plot_names"=plot_names)
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
    res <- results$summary
    overall <- list(c("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub))
    overall
}