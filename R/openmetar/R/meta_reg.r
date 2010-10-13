##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

binary.meta.regression <- function(binary.data, params, cov.name){
    cov.val.str <- paste("binary.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@studyNames,
                                level=params$conf.level, digits=params$digits, method="DL", 
                                mods=cov.vals)
    
                                                            
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    plot.data <- create.plot.data(binary.data, params, res, selected.cov=cov.name)
    plot.data$fitted.line <- fitted.line
    plot.path <- "./r_tmp/reg_plot.png"
    meta.regression.plot(plot.data, plot.path)   
    images <- c("regression plot"=plot.path)
    plot.names <- c("forest plot"="reg.plot")
    
    results <- list("images"=images, "coefficients"=betas, "plot_names"=plot.names)
    results
}