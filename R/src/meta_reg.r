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
    regDisp <- createRegressionDisp(res, params)
    regDisp
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    plot.data <- create.plot.data.binary(binary.data, params, res, selected.cov=cov.name)
    plot.data$fitted.line <- fitted.line
    plot.path <- "./r_tmp/reg_plot.png"
    meta.regression.plot(plot.data, plot.path)   
    images <- c("regression plot"=plot.path)
    plot.names <- c("forest plot"="reg.plot")
    results <- list("images"=images, "Summary"=regDisp, "plot_names"=plot.names)
    results
}


## TODO there's a lot of redundancy between this method
# and the above (binary.meta.regression) function --
# should probably refactor
cont.meta.regression <- function(cont.data, params, cov.name){
    res<-rma.uni(yi=contData@y, sei=contData@SE, 
                                        slab=contData@studyNames,
                                        method=params$rm.method, level=params$conf.level,
                                        digits=params$digits)
    
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
                                        
} 