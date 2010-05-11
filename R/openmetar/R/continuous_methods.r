####################################
# OpenMeta[Analyst]                                             #
# ----                                                                       #
# continuous_methods.r                                       # 
# Facade module; wraps methods that perform    #
# analysis on continuous data in a coherent        #
# interface.                                                            #
####################################

library(metafor)

#######################
#  continuous random effects  #
#######################
continuous.random <- function(contData, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(contData))) stop("Continuous data expected.")
    
    # call out to the metafor package
    if (length(contData@mean1) > 0) {
        res<-rma.uni(n1i=contData@N1, n2i=contData@N2, 
                                    m1i=contData@mean1, m2i=contData@mean2, slab=contData@studyNames,
                                    method=params$rm.method, measure=params$measure,
                                    level=params$conf.level, digits=params$digits)
    }
    else{
       res<-rma.uni(yi=contData@y, sei=contData@SE, 
                                    slab=contData@studyNames,
                                    method=params$rm.method, level=params$conf.level,
                                    digits=params$digits)
    }
    
    #
    # generate forest plot 
    #
    getwd()
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest.rma(res, digits=params$digits)
    dev.off()

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("forest plot"=forest_path)
    plot_names <- c("forest plot"="forest_plot")
    
    results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
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