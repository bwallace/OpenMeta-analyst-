##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta-reg.r                                                    #
##################################################################

library(metafor)

binary.meta.regression <- function(binary.data, params, cov.name){
    cov.val.str <- paste("binary.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@studyNames,
                                level=params$conf.level, digits=params$digits, method="DL", add=params$adjust,
                                to=params$to, mods=cov.vals)
    
                                                            
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    plot.data <- create.plot.data(binary.data, params, res, selected.cov=cov.name)
    plot.data$fitted.line <- fitted.line
    meta.regression.plot(plot.data, "yodog.png")   
}