#############################################
# OpenMeta[Analyst]                         #
# ----                                      #
# binary_methods.r                          # 
# Facade module; wraps methods that perform #
# analysis on binary data in a coherent     #
#  interface.                               #
#############################################


library(metafor)


####################################################
#  binary fixed effects -- inverse variance        #
####################################################
binary.fixed.inv.var <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  

    # call out to the metafor package
    res<-rma.uni(yi=binaryData@y, sei=binaryData@SE, slab=binaryData@studyNames,
                            level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                            to=params$to)
     
                                              
    # generate forest plot 
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest_plot<-forest.rma(res, digits=params$digits)
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
    
    
    ###
    # should we return the name of the result object & the name of the
    # plotting function as well here? perhaps only for the forest plot? 
    # this would allow interactive plot refinement via the console...
    results <- list("images"=images, "summary"=res, "plot_names"=plot_names)
    results
}

                                
binary.fixed.inv.var.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    apply_adjustment_to = c("only0", "all")
    
    params <- list("measure"=binary_metrics, "conf.level"="float", "digits"="float", 
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("measure"="OR", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("measure", "conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.inv.overall <- function(results){
    res <- results$summary
    overall <- list("estimate"=res$b[1], "lower"=res$ci.lb, "upper"=res$ci.ub)
    overall
}

########################################################
#       binary fixed effects -- mantel haenszel        #
########################################################
binary.fixed.mh <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  

    res<<-rma.mh(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                            ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                            level=params$conf.level, digits=params$digits)              
                                              
    #
    # generate forest plot 
    #
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest_plot<-forest.rma(res, digits=params$digits)
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

                                
binary.fixed.mh.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    apply_adjustment_to = c("only0", "all")
    
    params <- list("measure"=binary_metrics, "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("measure"="OR", "conf.level"=95, "digits"=3, "adjust"=.5, "to"="only0")
    
    var_order = c("measure", "conf.level", "digits", "adjust", "to")
    
    # constraints
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

binary.fixed.mh.is.feasible <- function(binaryData){
    # only feasible if we have raw (2x2) data for all studies
    length(binaryData@g1O1)==length(binaryData@g1O2) &&
    length(binaryData@g1O2)==length(binaryData@g2O1) &&
    length(binaryData@g2O1)==length(binaryData@g2O2) &&
         length(binaryData@g1O1) > 0
}

##################################################
#       binary fixed effects -- Peto             #
##################################################
binary.fixed.peto <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")  

    res<<-rma.peto(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                            ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                            level=params$conf.level, digits=params$digits)              
                                              
    #
    # generate forest plot 
    #
    forest_path <- "./r_tmp/forest.png"
    png(forest_path)
    forest_plot<-forest.rma(res, digits=params$digits)
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

                                
binary.fixed.peto.parameters <- function(){
    # parameters
    apply_adjustment_to = c("only0", "all")
    
    params <- list( "conf.level"="float", "digits"="float",
                            "adjust"="float", "to"=apply_adjustment_to)
    
    # default values
    defaults <- list("conf.level"=95, "digits"=3, "adjust"=.5)
    
    var_order = c("conf.level", "digits", "adjust", "to")
    
    parameters <- list("parameters"=params, "defaults"=defaults,  "var_order"=var_order)
}

binary.fixed.peto.is.feasible <- function(binaryData){
    # only feasible if we have raw (2x2) data for all studies
    length(binaryData@g1O1)==length(binaryData@g1O2) &&
    length(binaryData@g1O2)==length(binaryData@g2O1) &&
    length(binaryData@g2O1)==length(binaryData@g2O2) &&
         length(binaryData@g1O1) > 0
}

 

##################################
#       binary random effects    #
##################################
binary.random <- function(binaryData, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")
    
    # call out to the metafor package
    if (length(binaryData@g1O1) > 0) {
        res<-rma.uni(ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                    ci=binaryData@g2O1, di=binaryData@g2O2, slab=binaryData@studyNames,
                                    method=params$rm.method, measure=params$measure,
                                    level=params$conf.level, digits=params$digits)
    }
    else{
       res<-rma.uni(yi=binaryData@y, sei=binaryData@SE, 
                                    slab=binaryData@studyNames,
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


binary.random.parameters <- function(){
    # parameters
    binary_metrics <- c("OR", "RR", "RD")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "measure"=binary_metrics, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "measure"="OR", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "measure", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}





