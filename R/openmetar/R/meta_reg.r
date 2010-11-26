##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

binary.meta.regression <- function(reg.data, params, cov.name){
    cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.vals)
    reg.disp <- create.regression.disp(res, params)
    reg.disp
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    #reg.path <- paste(params$fp_outpath, sep="")
    plot.path <- "./r_tmp/reg.png"
    plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
    meta.regression.plot(plot.data, outpath=plot.path, symSize=1,
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