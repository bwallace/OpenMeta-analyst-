##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

meta.regression <- function(reg.data, params) {
   cov.data <- extract.cov.data(reg.data)
   cov.array <- cov.data$cov.array
   
   # remove when and if method dialog is added
   method <- params$method

   res<-try(rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits,
                                method=method, mods=cov.array))
   if (class(res)[1] != "try-error") {
       display.data <- cov.data$display.data
       reg.disp <- create.regression.display(res, params, display.data)
   
       if (length(display.data$n.cont.covs)==1 & length(display.data$factor.n.levels)==0) {
            # if only 1 continuous covariate, create reg. plot
            betas <- res$b
            fitted.line <- list(intercept=betas[1], slope=betas[2])
            plot.path <- "./r_tmp/reg.png"
            plot.data <- create.plot.data.reg(reg.data, params, fitted.line)

            # @TODO x and y labels ought to be passed in, probably
            plot.data$xlabel <- reg.data@covariates[[1]]@cov.name
            scale.str <- get.scale(params)
            if ((scale.str=="standard") || (scale.str=="arcsine")) {
                scale.str <- ""
                # This is for the y-axis label on regression plot - don't add "standard" or "arcsine" to label.
            }
            plot.data$ylabel <- paste(scale.str, " ", pretty.metric.name(as.character(params$measure)), sep="")
            meta.regression.plot(plot.data, plot.path)
            
            # write the plot data to disk so we can save it
            # @TODO will want to write the params data, too,
            # eventually
            plot.data.path <- save.plot.data(plot.data)

            images <- c("Regression Plot"=plot.path)
            plot.names <- c("reg.plot"="reg.plot")
            reg.plot.params.path <- save.plot.data(plot.data)
            plot.params.paths <- c("Regression Plot"=plot.data.path)

            results <- list("images"=images, "Summary"=reg.disp, "plot_names"=plot.names,
                              "plot_params_paths"=plot.params.paths)

        } else {
            results <- list("Summary"=reg.disp)
        }
    } else {
        results <- res
    }
    results
}

extract.cov.data <- function(reg.data) {
  # separate continuous and factor covariates and extract data.
  # The following are passed to create.regression.display
  n.cont.covs <- 0
  factor.n.levels <- NULL # vector containing number of levels for each factor covariate
  factor.cov.display.col <- NULL
  levels.display.col <- NULL
  studies.display.col <- NULL
  
  # initialize names of continuous covariates to empty list
  cont.cov.names <- c()
  cont.cov.array <- NULL
  for (n.covs in 1:length(reg.data@covariates)) {
    # put covariate data into two arrays, for continuous and factor covariates.
    cov <- reg.data@covariates[[n.covs]]
    cov.name <- cov@cov.name
    cov.vals <- cov@cov.vals
    cov.type <- cov@cov.type
    ref.var <- cov@ref.var
    if (cov.type=="continuous") {
      cov.col <- array(cov.vals, dim=c(length(reg.data@y), 1), 
                    dimnames=list(NULL, cov.name))
      cont.cov.array <- cbind(cont.cov.array, cov.col)
      cont.cov.names <- c(cont.cov.names, cov.name)
      n.cont.covs <- n.cont.covs + 1
    }
    factor.cov.array <- NULL
    if (cov.type=="factor") {
      levels <- unique(cov.vals)
      # Remove "" from levels, if necessary.
      levels.minus.NA <- setdiff(levels, "")
      # Levels except for reference variable
      levels.minus.ref.var <- setdiff(levels.minus.NA, ref.var)
      cov.cols <- array(dim=c(length(reg.data@y), length(levels.minus.ref.var)))
      studies.col <- c(sum(cov.vals==ref.var))
      for (col.index in 1:length(levels.minus.ref.var)) {
           level <- levels.minus.ref.var[col.index]
           cov.cols[cov.vals!="" & cov.vals!=level, col.index] <- 0
           cov.cols[cov.vals!="" & cov.vals==level, col.index] <- 1
           studies.col <- c(studies.col, sum(cov.vals==level)) 
      }
      factor.cov.array <- cbind(factor.cov.array, cov.cols)
      factor.n.levels <- c(factor.n.levels, length(levels.minus.NA))
      factor.cov.display.col <- c(factor.cov.display.col, cov.name, rep("",length(levels.minus.ref.var)))
      factor.studies.display.col <- c() 
      levels.display.col <- c(levels.display.col, ref.var, levels.minus.ref.var)
      studies.display.col <- c(studies.display.col, studies.col)
      }
  }
  cov.array <- cbind(cont.cov.array, factor.cov.array)
  cov.display.col <- c("Intercept", cont.cov.names, factor.cov.display.col)
  levels.display.col <- c(rep("",length(cont.cov.names) + 1), levels.display.col)
  studies.display.col <- c(rep("",length(cont.cov.names) + 1), studies.display.col)
  display.data <- list(cov.display.col=cov.display.col, levels.display.col=levels.display.col,
                       studies.display.col=studies.display.col, factor.n.levels=factor.n.levels, n.cont.covs=n.cont.covs)
  cov.data <- list(cov.array=cov.array, display.data=display.data)
                   
}

binary.fixed.meta.regression <- function(reg.data, params){
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
                                  lpatern = "solid",
                                  plotregion = "n",
                                  mcolor = "black",
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
  res <-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.data)
  reg.disp <- create.regression.disp(res, params, cov.names=dimnames(cov.data)[[2]]) 
  results <- list("Summary"=reg.disp)
}
