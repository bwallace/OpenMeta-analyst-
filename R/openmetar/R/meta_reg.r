##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

meta.regression <- function(reg.data, params, cov.name){
    cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="DL", 
                                mods=cov.vals)
    reg.disp <- create.regression.disp(res, params)
    reg.disp
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
   
    plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
    plot.path <- "./r_tmp/reg.png"
    meta.regression.plot(plot.data, plot.path, symSize=1,
                                  lcol = "darkred",
                                  metric = "Effect size",
                                  xlabel= plot.data$covariate$varname,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE)   
    images <- c("Regression Plot"=plot.path)
    plot.names <- c("forest plot"="reg.plot")
    results <- list("images"=images, "Regression Summary"=reg.disp, "plot_names"=plot.names)
    results
}


## TODO there's a lot of redundancy between this method
# and the above (binary.meta.regression) function --
# should probably refactor
cont.meta.regression <- function(cont.data, params, cov.name){
    res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, 
                                        slab=cont.data@study.names,
                                        method=params$rm.method, level=params$conf.level,
                                        digits=params$digits)
    
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
                                        
} 