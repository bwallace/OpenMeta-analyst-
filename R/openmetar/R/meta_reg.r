##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

binary.fixed.meta.regression <- function(reg.data, params, cov.names){
  # meta regression for numerical covariates
  cov.data <- array(dim=c(length(reg.data@y), length(cov.names)), dimnames=list(NULL, cov.names))  
    for (cov.name in cov.names) {
      # extract matrix of covariates
       cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
       cov.vals <- eval(parse(text=cov.val.str))
       cov.data[,cov.name] <- cov.vals
    }     
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.data)
    reg.disp <- create.regression.disp(res, params, cov.names)
    if (length(cov.names)==1) {
        # if just 1 covariate, create reg. plot
        betas <- res$b
        fitted.line <- list(intercept=betas[1], slope=betas[2])
        plot.path <- "./r_tmp/reg.png"
        plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
        meta.regression.plot(plot.data, outpath=plot.path, symSize=1,
                                  lcol = "darkred",
                                  y.axis.label = "Effect size",
                                  xlabel= cov.name,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE)   
        images <- c("Regression Plot"=plot.path)
        plot.names <- c("forest plot"="reg.plot")
        results <- list("images"=images, "Summary"=reg.disp, "plot_names"=plot.names)
    } else {
        results <- list("Summary"=reg.disp)
    }
}

random.meta.regression <- function(reg.data, params, cov.name){
    cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, 
                                method=params$rm.method, 
                                mods=cov.vals)
    reg.disp <- create.regression.disp(res, params)
    reg.disp
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    # temporary fix until params$rp_outpath is added to the GUI
    if (is.null(params$rp_outpath)) {
        plot.path <- "./r_tmp/reg.png"
    }
    else {
        plot.path <- params$rp_outpath
    }
    plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
    meta.regression.plot(plot.data, outpath=plot.path, symSize=1,
                                  lcol = "darkred",
                                  y.axis.label = "Effect size",
                                  xlabel= cov.name,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE)   
    images <- c("Regression Plot"=plot.path)
    plot.names <- c("forest plot"="reg.plot")
    results <- list("images"=images, "Summary"=reg.disp, "plot_names"=plot.names)
    results
}

binary.random.meta.regression.parameters <- function(){
    # parameters
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

categorical.meta.regression <- function(reg.data, params, cov.names) {
  # meta-regression for categorical covariates 
  cov.data <- array()
  var.names <- NULL
  for (cov.name in cov.names) {
      # extract matrix of covariates
       cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
       groups <- eval(parse(text=cov.val.str))
       group.list <- unique(groups)
       array.tmp <- array(dim=c(length(reg.data@y), length(group.list)-1), dimnames=list(NULL, group.list[-1]))
       for (group in group.list[-1]) {
           array.tmp[,group] <- as.numeric(groups == group)
       }
       if (length(cov.data) > 1) {
           cov.data <- cbind(cov.data, array.tmp)
       } else {
           cov.data <- array.tmp
       }
  }
  res <- res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.data)
  reg.disp <- create.regression.disp(res, params, cov.names=dimnames(cov.data)[[2]]) 
  results <- list("Summary"=reg.disp)
}
