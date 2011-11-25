####################################
#                                  #
# OpenMeta[Analyst]                #
# ----                             #
# plotting.r                       # 
#                                  #
# Flexible forest plotting.        # 
#  (And more?)                     #
#                                  #
# This code due mostly to Issa     #
#   Dahabreh and Paul Trow       #    
####################################

# largely a generalization based on an example by
# Murrell P., "R graphics", Chapman & Hall

library("grid")

#################################################################
#   functions for creating plot data to pass to plot functions  #
#################################################################

create.plot.data.generic <- function(om.data, params, res, selected.cov=NULL){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    scale.str <- "standard"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale.str <- "logit"
    }
    if ("ContinuousData" %in% class(om.data)) {
        transform.name <-"continuous.transform.f"
    } else if ("DiagnosticData" %in% class(om.data)) {
        transform.name <- "diagnostic.transform.f"
    }  else if ("BinaryData" %in% class(om.data)) {
        transform.name <- "binary.transform.f"
    }
    plot.options <- set.plot.options(params)
 
    if (params$fp_plot_lb == "[default]") {
        # value can be set to string  [default] by the GUI or can be NULL 
        # if the function is called at the command line
        plot.options$plot.lb <- NULL
    } else {
        plot.lb <- eval(parse(text=paste("c(", params$fp_plot_lb, ")", sep="")))
        plot.options$plot.lb <- eval(call(transform.name, params$measure))$calc.scale(plot.lb)
    } 
    
    if (params$fp_plot_ub == "[default]")  {
        plot.options$plot.ub <- NULL
    } else {
        plot.ub <- eval(parse(text=paste("c(", params$fp_plot_ub, ")", sep="")))
        if (scale.str == "logit") {
          plot.ub <- min(1, plot.ub)
        }  
        plot.options$plot.ub <- eval(call(transform.name, params$measure))$calc.scale(plot.ub)
    } 
    
    plot.data <- list(label = c(paste(params$fp_col1_str, sep = ""), om.data@study.names, "Overall"),
                      types = c(3, rep(0, length(om.data@study.names)), 2),
                      scale = scale.str,
                      options = plot.options)         
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    y.overall <- res$b[1]
    lb.overall <- res$ci.lb[1]
    ub.overall <- res$ci.ub[1]
    y <- om.data@y
    lb <- y - mult*om.data@SE
    ub <- y + mult*om.data@SE
    
    y <- c(y, y.overall)
    lb <- c(lb, lb.overall)
    ub <- c(ub, ub.overall)
    
    y.disp <- eval(call(transform.name, params$measure))$display.scale(y)
    lb.disp <- eval(call(transform.name, params$measure))$display.scale(lb)
    ub.disp <- eval(call(transform.name, params$measure))$display.scale(ub)
    
    effects.disp <- list(y.disp=y.disp, lb.disp=lb.disp, ub.disp=ub.disp)
    # these values will be displayed on the plot
    plot.data$effects.disp <- effects.disp
    
    if (metric.is.logit.scale(params$measure)) {
        # in logit scale, pass data in display scale - no scaling on x-axis
        y <- y.disp
        lb <- lb.disp
        ub <- ub.disp
    }
    
    effects <- list(ES = y,
                    LL = lb,
                    UL = ub) 
    plot.data$effects <- effects
     
     # covariates
    if (!is.null(selected.cov)){
        cov.val.str <- paste("om.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }

    plot.data
}

metric.is.log.scale <- function(metric){
    metric %in% c(binary.log.metrics, diagnostic.log.metrics)    
}

metric.is.logit.scale <- function(metric) {
    metric %in% c(diagnostic.logit.metrics)
}    

create.plot.data.binary <- function(binary.data, params, res, selected.cov = NULL){
    
    plot.data <- create.plot.data.generic(binary.data, params, res, selected.cov=selected.cov)
        
    # if we have raw data, add it to plot.data
    if (length(binary.data@g1O1) > 0)  {
    
        plot.data$col3 <- list(nums = binary.data@g1O1, denoms = binary.data@g1O1 + binary.data@g1O2)
    }
    
    if (length(binary.data@g2O1) > 0) {
         plot.data$col4 <- list(nums = binary.data@g2O1, denoms = binary.data@g2O1 + binary.data@g2O2)
    }
  
    plot.data
}

create.plot.data.diagnostic <- function(diagnostic.data, params, res, selected.cov = NULL){

    plot.data <- create.plot.data.generic(diagnostic.data, params, res, selected.cov=selected.cov)
    plot.options <- plot.data$plot.options
    plot.options$show.y.axis <- FALSE
    # don't show y axis in diagnostic forest plots
    # if we have raw data, add it to plot.data
    if (length(diagnostic.data@TP) > 0) {
        raw.data <- list("TP"=diagnostic.data@TP, "FN"=diagnostic.data@FN, "TN"=diagnostic.data@TN, "FP"=diagnostic.data@FP)
        terms <- compute.diagnostic.terms(raw.data, params)
        plot.data$col3 <- list(nums=terms$numerator, denoms=terms$denominator)
        
        metric <- params$measure
        # create label for column 3 based on metric
        label <- switch(metric,
        # sensitivity
        Sens = "TP / (TP + FN)", 
        # specificity
        Spec = "TN / (FP + TN)",
        # pos. predictive value
        PPV =  "TP / (TP + FP)",
        #neg. predictive value
        NPV =  "TN / (TN + FN)",
        # accuracy
        Acc = "(TP + TN) / Tot",
        # positive likelihood ratio
        PLR = "(TP * Di-) / (FP * Di+)", 
        # negative likelihood ratio
        NLR = "(FN * Di-) / (TN * Di+)",
        # diagnostic odds ratio
        DOR = "(TP * TN) / (FP * FN)")
        #data.col <- format.raw.data.col(nums = terms$numerator, denoms = terms$denominator, label = label) 
        #plot.data$additional.col.data$cases = data.col
        plot.data$options$col3.str <- label
    }
    plot.data
}

create.plot.data.continuous <- function(cont.data, params, res, selected.cov = NULL){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    plot.data <- create.plot.data.generic  (cont.data, params, res, selected.cov=selected.cov)
    plot.data
}

create.plot.data.overall <- function(res, study.names, params, data.type, addRow1Space){
    scale.str <- "standard"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale.str <- "logit"
    }
    ## TO DO - don't really nead three transforms - the transform only depends on the measure.
    if (data.type == "continuous") {
      transform.name <- "continuous.transform.f"
    } else if (data.type == "diagnostic") {
      transform.name <- "diagnostic.transform.f"
    }  else if (data.type == "binary") {
      transform.name <- "binary.transform.f"
    }
    plot.options <- set.plot.options(params)
    plot.options$show.col3 <- FALSE
    plot.options$show.col4 <- FALSE
    # currently not displaying raw data cols. for overall plots
    
    if (params$fp_plot_lb == "[default]") {
        plot.options$plot.lb <- NULL
    } else {
        plot.lb <- eval(parse(text=paste("c(", params$fp_plot_lb, ")", sep="")))
        plot.options$plot.lb <- eval(call(transform.name, params$measure))$calc.scale(plot.lb)
    }
    if (params$fp_plot_ub == "[default]") {
        plot.options$plot.ub <- NULL
    } else {
        plot.ub <- eval(parse(text=paste("c(", params$fp_plot_ub, ")", sep="")))
        plot.options$plot.ub <- eval(call(transform.name, params$measure))$calc.scale(plot.ub)
    } 
    plot.options$fp.show.summary.line <- FALSE
    # turning off the summary line for cumulative and loo, as it doesn't make 
    # sense. This overrides the check box in forest plot options pane.
    if (metric.is.log.scale(params$measure)) {
        plot.options$show.y.axis <- FALSE
        # don't show y-axis for diagnostic forest plots
    } else {
        plot.options$show.y.axis <- TRUE
    }    
    if (addRow1Space == TRUE) {
        # Add space to row 1 for cumulative ma to align study names.
        study.names[1] <- paste("   ", study.names[1], sep="")
    }

    plot.data <- list( label = c(params$fp_col1_str, study.names),  
                       # add blank line to study.names to align with Overall row
                       types = c(3, rep(0, length(study.names)), 2),
                       scale = scale.str,
                       options = plot.options)
    # unpack data
    y <- NULL
    lb <- NULL
    ub <- NULL
    
    for (count in 1:length(study.names)) {
      # subtract because of blank line for overall
      y <- c(y, res[[count]]$b)
      lb <- c(lb, res[[count]]$ci.lb)
      ub <- c(ub, res[[count]]$ci.ub)
    }
      
    y.disp <- eval(call(transform.name, params$measure))$display.scale(y)
    lb.disp <- eval(call(transform.name, params$measure))$display.scale(lb)
    ub.disp <- eval(call(transform.name, params$measure))$display.scale(ub)                   
    effects.disp <- list(y.disp=y.disp, lb.disp=lb.disp, ub.disp=ub.disp)
    plot.data$effects.disp <- effects.disp
    
    if (metric.is.logit.scale(params$measure)) {
        # in logit scale, pass data in display scale - no scaling on x-axis
        y <- y.disp
        lb <- lb.disp
        ub <- ub.disp
    }
    
    effects <- list(ES = y,
                    LL = lb,
                    UL = ub) 
    plot.data$effects <- effects
    plot.data
}

# create subgroup analysis plot data
create.subgroup.plot.data.generic <- function(subgroup.data, params, data.type, selected.cov=NULL) {
    grouped.data <- subgroup.data$grouped.data
    res <- subgroup.data$results
    subgroup.list <- subgroup.data$subgroup.list
    scale.str <- "standard"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale.str <- "logit"
    }
    ## TO DO - don't really nead three transforms - the transform only depends on the measure.
    if (data.type == "continuous") {
      transform.name <- "continuous.transform.f"
    } else if (data.type == "diagnostic") {
      transform.name <- "diagnostic.transform.f"
    }  else if (data.type == "binary") {
      transform.name <- "binary.transform.f"
    }
    cur.res <- NULL
    y <- NULL
    lb <- NULL
    ub <- NULL
    label.col <- NULL
    types <- NULL
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    for (i in 1:length(subgroup.list)){
        # create plot data for each subgroup and concatenate results
        cur.res <- res[[i]]
        params.tmp <- params
        cur.y.overall <- cur.res$b[1]
        cur.lb.overall <- cur.res$ci.lb[1]
        cur.ub.overall <- cur.res$ci.ub[1]
        cur.y <- grouped.data[[i]]@y
        cur.lb <- cur.y - mult*grouped.data[[i]]@SE
        cur.ub <- cur.y + mult*grouped.data[[i]]@SE
        y <- c(y, cur.y, cur.y.overall)
        lb <- c(lb, cur.lb, cur.lb.overall)
        ub <- c(ub, cur.ub, cur.ub.overall) 
        types <- c(types, rep(0, length(grouped.data[[i]]@study.names)), 1)
        label.col <-c(label.col, grouped.data[[i]]@study.names, paste("Subgroup ", subgroup.list[i], sep=""))
    } 
    cur.res <- res[[length(subgroup.list) + 1]]
    cur.y.overall <- cur.res$b[1]
    cur.lb.overall <- cur.res$ci.lb[1]
    cur.ub.overall <- cur.res$ci.ub[1]
    y <- c(y, cur.y.overall)
    lb <- c(lb, cur.lb.overall)
    ub <- c(ub, cur.ub.overall)
    types <- c(3,types, 2)
    label.col <- c(as.character(params$fp_col1_str), label.col, "Overall")
    plot.options <- set.plot.options(params)
    if (params$fp_plot_lb == "[default]") {
        plot.options$plot.lb <- NULL
    } else {
        plot.lb <- eval(parse(text=paste("c(", params$fp_plot_lb, ")", sep="")))
        plot.options$plot.lb <- eval(call(transform.name, params$measure))$calc.scale(plot.lb)
    }
    if (params$fp_plot_ub == "[default]") {
        plot.options$plot.ub <- NULL
    } else {
        plot.ub <- eval(parse(text=paste("c(", params$fp_plot_ub, ")", sep="")))
        plot.options$plot.ub <- eval(call(transform.name, params$measure))$calc.scale(plot.ub)
    }

    # should we show summary line for subgroup plots??
    plot.data <- list(label = label.col,
                      types=types,
                      scale = scale.str,
                      options = plot.options)      
    y.disp <- eval(call(transform.name, params$measure))$display.scale(y)
    lb.disp <- eval(call(transform.name, params$measure))$display.scale(lb)
    ub.disp <- eval(call(transform.name, params$measure))$display.scale(ub)
    
    # these values will be displayed on the plot
    effects.disp <- list(y.disp=y.disp, lb.disp=lb.disp, ub.disp=ub.disp)
    plot.data$effects.disp <- effects.disp
    
    if (metric.is.logit.scale(params$measure)) {
        # in logit scale, pass data in display scale - no scaling on x-axis
        y <- y.disp
        lb <- lb.disp
        ub <- ub.disp
    }
    
    effects <- list(ES = y,
                    LL = lb,
                    UL = ub)
   
    plot.data$effects <- effects
    
    # covariates
    if (!is.null(selected.cov)){
        cov.val.str <- paste("om.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }
    plot.data
}

create.subgroup.plot.data.binary <- function(subgroup.data, params) {
    grouped.data <- subgroup.data$grouped.data
    plot.data <- create.subgroup.plot.data.generic(subgroup.data, params, data.type="binary") 

    # if we have raw data, add it to plot.data
    if (length(grouped.data[[1]]@g1O1) > 0) {
    
        plot.data$col3 <- list(nums = subgroup.data$col3.nums, denoms = subgroup.data$col3.denoms)
    }
    
    if (length(grouped.data[[1]]@g2O1) > 0) {
         plot.data$col4 <- list(nums = subgroup.data$col4.nums, denoms = subgroup.data$col4.denoms)
    }
    plot.data
}

create.subgroup.plot.data.diagnostic <- function(subgroup.data, params) {
    grouped.data <- subgroup.data$grouped.data
    plot.data <- create.subgroup.plot.data.generic(subgroup.data, params, data.type="diagnostic") 
    if (length(grouped.data[[1]]@TP) > 0) {
        raw.data <- list("TP"=diagnostic.data@TP, "FN"=diagnostic.data@FN, "TN"=diagnostic.data@TN, "FP"=diagnostic.data@FP)
        terms <- compute.diagnostic.terms(raw.data, params)
        plot.data$col3 <- list(nums = subgroup.data$col3.nums, denoms = subgroup.data$col3.denoms)
        
        metric <- params$measure
        # create label for column 3 based on metric
        label <- switch(metric,
        # sensitivity
        Sens = "TP / (TP + FN)", 
        # specificity
        Spec = "TN / (FP + TN)",
        # pos. predictive value
        PPV =  "TP / (TP + FP)",
        #neg. predictive value
        NPV =  "TN / (TN + FN)",
        # accuracy
        Acc = "(TP + TN) / Tot",
        # positive likelihood ratio
        PLR = "(TP * Di-) / (FP * Di+)", 
        # negative likelihood ratio
        NLR = "(FN * Di-) / (TN * Di+)",
        # diagnostic odds ratio
        DOR = "(TP * TN) / (FP * FN")
        #data.col <- format.raw.data.col(nums = terms$numerator, denoms = terms$denominator, label = label) 
        #plot.data$additional.col.data$cases = data.col
        plot.data$options$col3.str <- label
    }
    plot.data
}

create.subgroup.plot.data.cont <- function(subgroup.data, params) {
    grouped.data <- subgroup.data$grouped.data
    plot.data <- create.subgroup.plot.data.generic(subgroup.data, params, data.type="continuous") 
}

# create regression plot data
create.plot.data.reg <- function(reg.data, params, fitted.line) {
     scale.str <- "standard"
     cov.name <- reg.data@covariates[[1]]@cov.name
     cov.vals <- reg.data@covariates[[1]]@cov.vals
     plot.data <- list("fitted.line" = fitted.line,
                      types = c(rep(0, length(reg.data@study.names))),
                      scale = scale.str,
                      covariate = list(varname = cov.name, values = cov.vals)
                       )
     alpha <- 1.0-(params$conf.level/100.0)
     mult <- abs(qnorm(alpha/2.0))
    
     
     y <- reg.data@y
     se <- reg.data@SE
     effects <- list(ES = y,
                    se = se)
     plot.data$effects <- effects
     plot.data
}

set.plot.options <- function(params) {
    # set default plot options
    plot.options <- list()
    # xticks is a vector of tick marks for the x-axis
    if (params$fp_xticks == "[default]") {
        params$fp_xticks <- NULL
    } else {
        plot.options$xticks <- eval(parse(text=paste("c(", params$fp_xticks, ")", sep="")))
    }
    if (params$fp_show_col1=='TRUE') {
      plot.options$show.study.col <- TRUE
    } else {
      plot.options$show.study.col <- FALSE
    }
    plot.options$col1.str <- as.character(params$fp_col1_str)
    
    if (params$fp_show_col2=='TRUE') {
      plot.options$show.col2 <- TRUE
    } else {
      plot.options$show.col2 <- FALSE
    }
    plot.options$col2.str <- as.character(params$fp_col2_str)
    
    if (params$fp_show_col3=='TRUE') {
      plot.options$show.col3 <- TRUE
    } else {
      plot.options$show.col3 <- FALSE
    }
    if (!is.null(params$fp_col3_str)) {
       plot.options$col3.str <- as.character(params$fp_col3_str)
    }
    
    if (params$fp_show_col4=='TRUE') {
      plot.options$show.col4 <- TRUE
    } else {
      plot.options$show.col4 <- FALSE
    }
    if (!is.null(params$fp_col4_str)) {
       plot.options$col4.str <- as.character(params$fp_col4_str)
    }
    
    # xlabel is the label for the x-axis
    if (params$fp_xlabel == "[default]") {
        plot.options$xlabel <- pretty.metric.name(as.character(params$measure))
    } else {
        plot.options$xlabel <- as.character(params$fp_xlabel)
    }
    # if show.summary.line is TRUE, a vertical dashed line is displayed at the
    # overall summary.
    if (params$fp_show_summary_line=='TRUE') {  
      plot.options$show.summary.line <- TRUE
    } else {
      plot.options$show.summary.line <- FALSE
    }
    plot.options$show.y.axis <- TRUE
    
    plot.options$digits <- params$digits
    plot.options
}    

pretty.metric.name <- function(metric) {
  # labels for x-axis of forest plot
  metric.name <- switch(metric,
        OR = "Odds Ratio",
        RD = "Risk Difference",
        RR = "Risk Ratio",
        AS = "Arcsine Risk Difference",
        PR = "Proportion",
        PLN = "Log Proportion",  
        PLO = "Logit Proportion",
        PAS = "Arcsine of Square Root Proportion",
        PET  = "Freeman-Tukey Double Arcsine Proportion",                
        PETO = "Peto",
        YUQ = "Yule's Q",
        YUY = "Yules Y",
        Sens = "Sensitivity", 
        Spec = "Specificity",
        # pos. predictive value
        PPV =  "Pos. predictive value",
        #neg. predictive value
        NPV =  "Neg. predictive value",
        # accuracy
        Acc = "Accuracy",
        # positive likelihood ratio
        PLR = "Pos. likelihood ratio", 
        # negative likelihood ratio
        NLR = "Neg. likelihood ratio",
        # diagnostic odds ratio
        DOR = "Diagnostic odds ratio")
}

###################################
#   functions for creating plots  #
###################################

# get data for the study column
study.column <- function(forest.data, title.font="bold") {
    content<-rep(NA, length(forest.data$label))
    for (i in 1:length(forest.data$label)){
      if (forest.data$types[i] !=  0)
        content[i] <- list(textGrob(forest.data$label[i], x=0, just = "left", gp = gpar(fontface = title.font, fontsize="10")))
      else
        content[i] <- list(textGrob(forest.data$label[i], x=0, just = "left", gp = gpar(fontface = "plain", fontsize="10")))
    }
    
    rows<-c(1, rep(NA, (length(forest.data$label)-1) ) )
    for (i in 1:(length(forest.data$label) -1)){
      if (forest.data$types[i] == 3  &&  forest.data$types[i+1] == 0 )
        rows[i+1] <- rows[i] + 2
      else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 2 )
        rows[i+1] <- rows[i]  + 1
      else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 1 )
        rows[i+1] <- rows[i] + 1
      else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 0 )
        rows[i+1] <- rows[i] + 2
      else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 2 )
        rows[i+1] <- rows[i] + 2
      else
       rows[i+1] <- rows[i] + 1
    }
    
    study.column.list <- list(content = content, rows = rows)
    
    study.column.list
}

# additional columns
additional.columns <- function(forest.data, font = "bold") {
    #  first get the number of columns
    #additional.data <- length(forest.data$additional.col.data)
    additional.columns <- vector("list", length(forest.data$additional.col.data))
    
    for (j in 1:length(forest.data$additional.col.data)){
        content<-rep(NA, length(forest.data$label))

        for (i in 1:length(forest.data$label)){
          if (forest.data$types[i] != 0)
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], 
                      x=1, just = "right", gp = gpar(fontface = font, fontfamily="mono", fontsize="10")))
          else
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], 
                      x=1, just = "right", gp = gpar(fontface = "plain", fontfamily="mono", fontsize="10")))
        }
        
        rows<-c(1, rep(NA, (length(forest.data$label)-1)))
        for (i in 1:(length(forest.data$label)-1)){
          if (forest.data$types[i] == 3  &&  forest.data$types[i+1] == 0 )
            rows[i+1] <- rows[i] + 2
          else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 2 )
            rows[i+1] <- rows[i] + 1
          else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 1 )
            rows[i+1] <- rows[i] + 1
          else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 0 )
            rows[i+1] <- rows[i] + 2
          else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 2 )
            rows[i+1] <- rows[i] + 2
          else
            rows[i+1] <- rows[i] + 1
        }
        additional.columns[[j]] <-list(content = content, rows = rows)
    }
    additional.columns
}

effectsize.column <- function(forest.data, box.sca = 1) {
    # calculate range of data to display
    rows<-c(1, rep(NA, (length(forest.data$effects$ES)) ) )
    for (i in 1:(length(forest.data$effects$ES))){
      if (forest.data$types[i] == 3  &&  forest.data$types[i+1] == 0)
        rows[i+1] <- rows[i] + 2
      else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 2)
        rows[i+1] <- rows[i]  + 1
      else if (forest.data$types[i] == 0  &&  forest.data$types[i+1] == 1)
        rows[i+1] <- rows[i] + 1
      else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 0)
        rows[i+1] <- rows[i] + 2
      else if (forest.data$types[i] == 1  &&  forest.data$types[i+1] == 2)
        rows[i+1] <- rows[i] + 2
      else
        rows[i+1] <- rows[i] + 1
    }
    # weights for the boxes
    # note that 1.96 is a convention [not necessary for the scaling]
    # the analysis functions determine the CI width (e.g. 95% or 99%)
    # this is just scaling the boxes according to the SE
    precision <- NULL
    effect.col.range <- NULL
    user.lb <- NULL
    user.ub <- NULL
    effect.col<-forest.data$effects
    # i have kept the "ifs" below: when we decide to include more metrics
    # these will be expanded
    
    if (forest.data$scale == "log"){
           precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    } else if (forest.data$scale == "standard") {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    } else if (forest.data$scale == "logit") {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    }
    
    effect.col.sizes <- box.sca * precision/max(precision)
    # sizes of the boxes in the forest plot - proportional to width of CI
    
    # Check whether user input's for plot lower and upper bounds are OK.
    plot.lb.max <- min(effect.col$ES)
    # smallest value for which we accept user's input for plot lower bound.
    # User's lower bound must be less thant all effect sizes.
    plot.ub.min <- max(effect.col$ES) 
    # largest user input for plot upper bound. All effect sizes must be less than this value.
    if (!is.null(forest.data$options$plot.lb)) {
        # user specifies the lower bound for the display range, so use it if not too large.
        if (forest.data$options$plot.lb < plot.lb.max) {
          user.lb <- forest.data$options$plot.lb
        }
    } 
    if (!is.null(forest.data$options$plot.ub)) {
        # user specifies just the upper bound for the display range, so use it if not too small
        if (forest.data$options$plot.ub > plot.ub.min) {
          user.ub <- forest.data$options$plot.ub
        }
    }
    
    if (is.null(user.lb) || is.null(user.ub)) {
    # if user has not supplied both lower and upper bounds (that meet the requirements), compute them
    # heuristically as effect.col.range.
      if (forest.data$scale == "logit") { 
        effect.col.range <- c(0, 1)
        # this is in standard scale.
        #effect.col.range <- c(min(effect.col$LL), max(effect.col$UL))
      }
      else {
          
        # left.distance <- mean(effect.col$ES) - min(effect.col$LL)
        #if (min(effect.col$LL)>0 && max(effect.col$UL)>0) {
        #  effect.col.range <- c(max(0.5*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) + 0.3, max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)<=0) { 
        #   effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(-1.5*max(effect.col$ES) + 0.3, max(effect.col$UL)))
           # if 
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)>0) { 
       #     effect.col.range <- c(max(5*min(effect.col$ES) , min(effect.col$LL)), min(5*max(effect.col$ES), max(effect.col$UL)))
       # } else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)>0 && max(effect.col$ES)>0) { 
        #   effect.col.range <- c(max(-2*min(effect.col$ES) , min(effect.col$LL)), min(1.5*max(effect.col$ES) + 0.3, max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)<0) { 
        #   effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(0.10*max(effect.col$ES) , max(effect.col$UL)))
        #} 
        
        # When scale is log or standard, this is a heuristic to determine a reasonable range for the displayed values - 
        # confidence intervals that exceed this range are truncated and left or right arrows are displayed instead of the full CI.
        effect.size.max <- max(effect.col$ES) 
        effect.size.min <- min(effect.col$ES)
        effect.size.width <- effect.size.max - effect.size.min
        
        effect.col.max <- max(effect.col$UL)
        effect.col.min <- min(effect.col$LL)
        arrow.factor <- 2
        # confidence intervals extend at most arrow.factor times effect.size.width beyond (effect.size.min, effect.size.max)
        effect.col.range.max <- min(effect.col.max, effect.size.max + arrow.factor * effect.size.width)
        effect.col.range.min <- max(effect.col.min, effect.size.min - arrow.factor * effect.size.width)
        
        effect.col.range <- c(effect.col.range.min, effect.col.range.max)
        # Origninal heuristics
        #if (min(effect.col$LL)>0 && max(effect.col$UL)>0) {
        #  effect.col.range <- c(max(0.5*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)<=0) { 
        #   effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(-1.5*max(effect.col$ES) , max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)>0) { 
        #   effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)>0 && max(effect.col$ES)>0) { 
        #   effect.col.range <- c(max(-2*min(effect.col$ES) , min(effect.col$LL)), min(1.5*max(effect.col$ES) , max(effect.col$UL)))
        #} else if (min(effect.col$LL)<=0 && max(effect.col$UL)<0) { 
        #   effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(0.10*max(effect.col$ES) , max(effect.col$UL)))
        #}
      }
      # this is an ugly solution to an uncommon problem
        
        merge.data <- data.frame(x = forest.data$types[-1][1:length(forest.data$effects$ES)], y = effect.col$LL, z = effect.col$UL)
        merge.data <- subset(merge.data, x>0)
        if (length(merge.data$y) > 0) {
          if (min(effect.col.range) >= min(merge.data$y)) { 
            effect.col.range[1] <- min(merge.data$y)
          }
        }
        if (length(merge.data$z) > 0) {
          if (max(effect.col.range) <= max(merge.data$z)) { 
            effect.col.range[2] <- max(merge.data$z)
          }
        }
    }
    if (! is.null(user.lb)) {
      # if the user's lb input is OK, set lower bound of range equal it.
      effect.col.range[1] <- user.lb
    }
    if (! is.null(user.ub)) {
      # if the user's ub input is OK, set upper bound of range equal it.
      effect.col.range[2] <- user.ub
    }

   list(ES = forest.data$effects$ES, LL = forest.data$effects$LL, 
                  UL = forest.data$effects$UL, rows = rows[-1], types = forest.data$types[-1][1:length(forest.data$effects$ES)],
                  range = effect.col.range,
                  sizes = effect.col.sizes)
}

create.plot.options <- function(forest.data, gapSize=3, plotWidth = 4) {
    
    effect.col.width <- unit(plotWidth, "inches")
    # width of the forest plot
    forest.params = list(
        col.gap = unit(gapSize, "mm"),
        effect.col.width = effect.col.width
    )
    forest.params
}

# Function to draw a cell in a text column
draw.label.col <- function(col, j) {
  # Insert data columns from forest.data$additional.col.data into the plot
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j))
    # Labels are grobs containing their location so just
    # have to grid.draw() them
    grid.draw(col$content[[i]])
    popViewport()
  }
}

# Function to draw a non-summary rect-plus-CI
draw.normal.CI <- function(LL, ES, UL, size) {
  # "native" units to position relative to
  # the x-axis scale, and "snpc" units to size relative to
  # the height of the row
  # ("snpc" stands for "square normalised parent coordinates"
  #  which means that the value is calculated as a proportion
  #  of the width and height of the current viewport and the
  #  physically smaller of these is used)
  grid.rect(x=unit(ES, "native"),
            width=unit(size, "snpc"), height=unit(size, "snpc"),
            gp=gpar(fill="black"))
  # Draw arrow if exceed col range
  # convertX() used to convert between coordinate systems
 
# TO DO: there is one case where this is a problem, when the summary estimate is wider than the CI
# this can happen when the summary is calculated in a subgroup where there is only one study
# this should be handled by another "if" that forces the xscale to be determined "primarily" by the CI of the summaries
# this has to be done in the function above 
  if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) >= 0){
    # this line is too long on the right - draw a right arrow from LL to 1 (in approriate coords.) 
    grid.arrows(x=unit(c(LL, 1), c("native", "npc")), length=unit(0.05, "inches"))                 
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) <= 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0){
    # this line is too long on the left - draw a left arrow from UL to 0 (in approriate coords.)
    grid.arrows(x=unit(c(UL, 0), c("native", "npc")),
                length=unit(0.05, "inches"))
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1   &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0 ){
    # this line is too long on both sides - draw a left arrow from ES to 0 and a right arrow from ES to 1 (in approriate coords.)
    grid.arrows(x=unit(c(ES, 0), c("native", "npc")), length=unit(0.05, "inches")) 
    grid.arrows(x=unit(c(ES, 1), c("native", "npc")), length=unit(0.05, "inches"))              
  }
  else {
    # this line is too short - draw white if totally inside rect
    line.col <- if ((convertX(unit(ES, "native") + unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) > UL) &&
                   (convertX(unit(ES, "native") - unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) < LL))
      "white"
    else
       # this line is just right - Goldilocks
      "black"
    grid.lines(x=unit(c(LL, UL), "native"), y=0.5,
               gp=gpar(col=line.col))
  }
}

# Function to draw a summary "diamond"
draw.summary.CI <- function(LL, ES, UL, size, color, diam.height) {
  # for diamonds: using half the height of the equivalent rect
  grid.polygon(x=unit(c(LL, ES, UL, ES), "native"),
               y=unit(0.5 + c(0, 0.25*diam.height*size, 0, -0.25*diam.height*size), "npc"), gp=gpar(fill=color))
}

###########################################
# Function to draw the forest plot graphs #
########################################### 
draw.data.col <- function(forest.data, col, j, color.overall = "black",
                          color.subgroup = "black", summary.line.col = "darkred",
                          summary.line.pat = "dashed",
                          x.axis.label, diam.size=1,
                          user.ticks,
                          show.y.axis) {
  pushViewport(viewport(layout.pos.col=j, xscale=col$range))

# This is the "null" line
# "ifs" left in as we will possibly expand this when new metrics become available
# note that if the line extends outside the xscale bounds, it will be 
# truncated and replaced with a left or right arrow (or both).
  if (show.y.axis == TRUE) {
    if (forest.data$scale == "log" && min(col$range)<0 && max(col$range)>0 ) {
      grid.lines(x=unit(0, "native"), y=0:1)
    }
    if (forest.data$scale == "standard" && min(col$range)<0 && max(col$range)>0 ) { 
      grid.lines(x=unit(0, "native"), y=0:1)
    }
    if (forest.data$scale == "logit" && min(col$range)<0 && max(col$range)>0 ) { 
        grid.lines(x=unit(0, "native"), y=0:1)
    }
  }
  
  if (!is.null(forest.data$options$show.summary.line)) {
    if (forest.data$options$show.summary.line == TRUE) {
          # draw vertical line for summary
        grid.lines(x=unit(col$ES[length(col$ES)], "native"),
             y=0:1, gp=gpar(lty = summary.line.pat, col= summary.line.col))
    }
  }  
  
  if  (forest.data$scale == "standard") {
    if (length(user.ticks) == 0) {
      grid.xaxis(gp=gpar(cex=0.6))
    } else {+6
        grid.xaxis(at = user.ticks , label = user.ticks, gp=gpar(cex=0.6))
      }
    }
    
  if (forest.data$scale == "log")  {
    if (length(user.ticks) == 0) { # some cheap tricks to make the axis ticks look nice (in most cases)...
            to.make.ticks <- range(exp(col$range))
            ticks <- axTicks(1, axp=c(to.make.ticks, 3), usr=c(-100, 100), log=TRUE)
  	        log.ticks <- log(ticks)
		        log.ticks <- log.ticks[log.ticks > min(col$range) - 0.5]    # remember it is additive on this scale
            log.ticks <- log.ticks[log.ticks < max(col$range) + 0.5]
            ticks <- exp(log.ticks)
    } else {
		        ticks <- user.ticks
            log.ticks <- log(user.ticks)
    }
        grid.xaxis(at = log.ticks , label = round(ticks, 3), gp=gpar(cex=0.6))          
  } 
  if (forest.data$scale == "logit")  {
        if (length(user.ticks) == 0) { # some cheap tricks to make the axis ticks look nice (in most cases)...
            ticks <- c(0, .25, .5, .75, 1)
        } else {
		        ticks <- user.ticks
        }
        
        grid.xaxis(at = ticks , label = ticks, gp=gpar(cex=0.6))          
  } 
        
  grid.text(x.axis.label, y=unit(-2, "lines"), gp=gpar(cex=0.8))
  popViewport()
  for (i in 1:length(col$rows)) {
    pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j,
                          xscale=col$range))   
    if (col$types[i] == 0){
       draw.normal.CI(col$LL[i], col$ES[i], col$UL[i], col$sizes[i])
    }
    else if (col$types[i] == 1){
       draw.summary.CI(col$LL[i], col$ES[i], col$UL[i], col$sizes[i], color.subgroup, diam.size )
    }
    else if (col$types[i] == 2){
       draw.summary.CI(col$LL[i], col$ES[i], col$UL[i], col$sizes[i], color.overall, diam.size )
    }
    popViewport()
  }
}
 
#######################################
#            forest plot              #
####################################### 
forest.plot <- function(forest.data, outpath) {
  forest.data <- format.data.cols(forest.data)
  # format the data columns displayed on fores plot
  show.study.col <- forest.data$options$show.study.col
  viewport.data <- forest.plot.data(forest.data, just="left")
  viewport.layout <- viewport.data$vp.layout
  how.wide <- viewport.data$how.wide
  height <- viewport.data$height
  how.tall <- convertY(unit(rep(1, height)  , "lines") , "inches" , valueOnly=TRUE)

  # so here we're just going to use the relatively hacky 
  # strategy of (R-)grepping for the literal ".png"
  # note that this means that, technically, if someone tries 
  # to save an iamge to my.pngimg.pdf, it will save it instead
  # as a png. on the other hand, why would someone do that?
  if (length(grep(".png", outpath)) != 0){
      png(file=outpath, width = how.wide+1, height = height*how.tall+2 , units = "in", res = 144) 
  }
  else{
      pdf(file=outpath, width = how.wide+1, height = height*how.tall+2) 
  }
                        
  pushViewport(viewport(layout=viewport.layout))
  draw.forest.plot(forest.data, viewport.data)
  
  graphics.off()
}


 
#######################################
#      forest plot data               #
####################################### 
 forest.plot.data <- function(forest.data, just){
    # Calculates width and height for viewport and output file
    show.study.col <- forest.data$options$show.study.col
    study.col <- study.column(forest.data, "bold")
    additional.cols <- c()
    if (length(forest.data$additional.col.data)>0 ){
        additional.cols <- additional.columns(forest.data, "bold")    
    } 
    effect.col <- effectsize.column(forest.data, box.sca=0.8)
    # return the LL, ES, and UL and range of data to display
    forest.plot.params <- create.plot.options(forest.data, gapSize = 3.2, plotWidth=5)

    # these are calls to plotting functions
    if (forest.data$types[length(forest.data$types)] != 0) {
      # last row is a summary
        extra.space <- sum(forest.data$types != 0) 
    } else {
      # add a little more space because last row is not a summary
        extra.space <- sum(forest.data$types != 0) + 1 
    }   
    height <- length(forest.data$types) + extra.space
  
    if (length(forest.data$additional.col.data)>0 )      {         # first if additional colums are present
        width.list <-vector("list")
        width.list[[1]] <- unit.c(max(unit(rep(1, length(forest.data$label)), 
                               "grobwidth", additional.cols[[1]]$content)), forest.plot.params$col.gap)
        if  (length(forest.data$additional.col.data)>1 )  {   
            for (i in 2:length(additional.cols))  {
            width.list[[i]] <- unit.c(width.list[[i-1]], max(unit(rep(1, length(forest.data$label)), 
                                   "grobwidth", additional.cols[[i]]$content)), forest.plot.params$col.gap) 
                 }
        }
        if (show.study.col==TRUE) {
          how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
                          "grobwidth", study.col$content)), "inches" , valueOnly=TRUE  ) +
                      convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )  +
                      sum( convertX(   unit.c(width.list[[length(additional.cols)]]) , "inches" , valueOnly=TRUE ) )  + 
                      length(additional.cols)*   convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )   + 
                      convertX(forest.plot.params$effect.col.width, "inches" , valueOnly=TRUE ) 
        } else {  
          how.wide <- convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )  +
                      sum( convertX(   unit.c(width.list[[2:length(additional.cols)]]) , "inches" , valueOnly=TRUE ) )  + 
                      length(additional.cols)*   convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )   + 
                      convertX(forest.plot.params$effect.col.width, "inches" , valueOnly=TRUE )
        }
        
        #how.tall <- convertY(unit(rep(1, height)  , "lines"), "inches" , valueOnly=TRUE )
        vp.width = unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                              forest.plot.params$col.gap,  width.list[[length(additional.cols)]]  ,  forest.plot.params$effect.col.width)
        vp.layout <- grid.layout(height , 2*length(additional.cols)+3,
                              width=vp.width,
                              height = unit(rep(1, height)  , "lines"),
                              just=just)
    }   else  { # if no additional columns things are simple
          how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
                                   "grobwidth", study.col$content)), "inches" , valueOnly=TRUE  ) +
                      convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )  +
                      convertX(forest.plot.params$effect.col.width, "inches" , valueOnly=TRUE ) 
        #how.tall <- convertY(unit(rep(1, height)  , "lines") , "inches" , valueOnly=TRUE ) 
        vp.width=unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                                  forest.plot.params$col.gap,   forest.plot.params$effect.col.width)
        
        vp.layout <- grid.layout(height , 2*length(additional.cols)+3,
                              width=vp.width,
                              height = unit(rep(1, height)  , "lines"),
                              just=just)
    }
    vp.data <- list("how.wide"=how.wide, "height"=height,
                          "vp.layout"=vp.layout)
}

#######################################
#            draw forest plot         #
####################################### 
draw.forest.plot <- function(forest.data, viewport.data){
    # Draws forest plots using viewport data extracted by forest.plot.data
    show.study.col <- forest.data$options$show.study.col                             
    how.wide <- viewport.data$how.wide
    #how.tall <- vp.data$how.tall
    height <- viewport.data$height
    viewport.layout <- viewport.data$vp.layout
    # these are calls to data functions
    study.col <- study.column(forest.data, "bold")
    additional.cols <- c()
    if (length(forest.data$additional.col.data)>0 ){
        additional.cols <- additional.columns(forest.data, "bold")    
    } 
    effect.col <- effectsize.column(forest.data, box.sca=0.8)
    # return the LL, ES, and UL and range of data to display
    forest.plot.params <- create.plot.options(forest.data, gapSize = 3.2, plotWidth=5)
    
    # Draw the text in study col and additional cols
    if (show.study.col==TRUE) {
      draw.label.col(study.col, 1)
    }
    if (length(additional.cols)>0 )  {
      for (i in 1:length(additional.cols)){
               draw.label.col(additional.cols[[i]], 1+2*i)
      }
    }  
   
    xticks <- forest.data$options$xticks
      
    draw.data.col(forest.data, effect.col, 2*length(additional.cols)+3,
                             color.overall = "lightblue",
                             color.subgroup = "yellow",
                             summary.line.col= "red",
                             summary.line.pat = "dashed",
                             x.axis.label = forest.data$options$xlabel,
                             diam.size = 1.2,
                             user.ticks = xticks,
                             show.y.axis = forest.data$options$show.y.axis)
}

#######################################
#            two forest plots         #
#######################################
 
two.forest.plots <- function(forest.data1, forest.data2, outpath) {
   # draw two forest plots side by side.
   forest.data1 <- format.data.cols(forest.data1)
   forest.data2 <- format.data.cols(forest.data2)
   viewport.data1 <- forest.plot.data(forest.data1, just="left")     
   viewport.data2 <- forest.plot.data(forest.data2, just="right")
   viewport.layout1 <- viewport.data1$vp.layout
   viewport.layout2 <- viewport.data2$vp.layout
   how.wide1 <- viewport.data1$how.wide
   if (forest.data2$options$show.study.col == TRUE) {
     how.wide2 <- viewport.data2$how.wide + .75
     # add some more space if to the right graph if showing study cols.
   } else {
     how.wide2 <- viewport.data2$how.wide
   }
   height1 <- viewport.data1$height
   height2 <- viewport.data2$height
   height <- max(height1, height2)
   how.tall <- convertY(unit(rep(1, height)  , "lines") , "inches" , valueOnly=TRUE )
   pushViewport(viewport(layout=grid.layout(1,2), width=how.wide1 + how.wide2))               
   

   if (length(grep(".png", outpath)) != 0){
      png(file=outpath, width = how.wide1 + how.wide2, height = height*how.tall+1 , units = "in", res = 144) 
   }
   else{
      pdf(file=outpath, width = how.wide1 + how.wide2 + 1, height = height*how.tall+2) 
   }
                                    
   pushViewport(viewport(layout=viewport.layout1, layout.pos.col=1))
   draw.forest.plot(forest.data1, viewport.data1)   
   popViewport()
   pushViewport(viewport(layout=viewport.layout2,layout.pos.col=2))
   draw.forest.plot(forest.data2, viewport.data2)
   popViewport()
   
   graphics.off()
}

#######################################
#       meta-regression scatter       #
#######################################
meta.regression.plot <- function(plot.data, outpath,
                                  symSize=1,
                                  lcol = "darkred",
                                  y.axis.label = "Effect size",
                                  xlabel= plot.data$covariate$varname,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE) {


    # make the data data.frame
    data.reg <- data.frame(plot.data$effects, types = plot.data$types)
    # data for plot (only keep the studies - not the summaries)
    data.reg <- subset(data.reg, types==0)
    cov.name <- plot.data$covariate$varname
    cov.values <- plot.data$covariate$values
    # calculate radii of circles
    se <- plot.data$effects$se
    inv.var <- 1 / se^2
    max.symbol.size <- (max(cov.values) - min(cov.values)) / 10
    # radius of the largest circle
    max.ratio <- 10
    # ratio of radii of largest circle to smallest circle
    radii <- calculate.radii(inv.var, max.symbol.size, max.ratio)
    # radii are scaled by a function of the form C x^exp, where 
    # exp = 1 if the ratio of the maximum of inv.var to the minimum of inv.var is
    # less than max.ratio. Otherwise, exp < 1 and C are calculated from 
    # max.ratio and the max and min of inv.var. 
    png(file=outpath, width=5 , height=5, units="in", res=144)
    
    #depends on whether these are natural or log
    if (plot.data$scale == "standard"){
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches=FALSE,
              xlab = xlabel, ylab = y.axis.label, bty = plotregion, fg = mcolor)
    }
    else{ 
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches = FALSE,
              xlab = xlabel, ylab = y.axis.label, bty = plotregion, fg = mcolor)
    }
    # note that i am assuming you have
    #the untransformed coefficient from the meta-reg
    # so i am doing no transformation
    if (regline == TRUE)  {
       x<-c(min(cov.values), max(cov.values))
       y<-c (plot.data$fitted.line$intercept + 
                min(cov.values)*plot.data$fitted.line$slope, plot.data$fitted.line$intercept + 
                max(cov.values)*plot.data$fitted.line$slope)
       lines(x, y, col = lcol, lwd = lweight, lty = lpatern)
    }
    graphics.off()
}

#######################################
#       Diagnostic SROC               #
#######################################

sroc.plot <- function(plot.data, outpath,
                      symSize=1,
                      lcol = "darkred",
                      lweight = 3,
                      lpatern = "dotted",
                      plotregion = "n",
                      mcolor = "darkgreen") {
 
    fitted.line <- plot.data$fitted.line
    weighted <- plot.data$weighted
    TPR <- plot.data$TPR
    FPR <- plot.data$FPR
    s.range <- plot.data$s.range
    
    png(file=outpath, width=5 , height=5, units="in", res=144)
    if (weighted == "weighted") {
        inv.var <- plot.data$inv.var    
        max.symbol.size <- .05
        # radius of the largest circle
        max.ratio <- 10
        # ratio of radii of largest circle to smallest circle
        radii <- calculate.radii(inv.var, max.symbol.size, max.ratio)
        symbols(y = plot.data$TPR, x = plot.data$FPR, circles = symSize*radii, inches=FALSE,
              xlab = "FPR", ylab = "TPR", xlim = c(0,1), ylim = c(0,1), bty = plotregion, fg = mcolor)
    } else {
        radii <- .01*rep(1, length(TPR))
        symbols(y = plot.data$TPR, x = plot.data$FPR, squares = radii, inches=FALSE,
              xlab = "FPR", ylab = "TPR", xlim = c(0,1), ylim = c(0,1), bty = plotregion, fg = mcolor)     
    }
    # create regression line values
    s.vals <- seq(from = s.range$min, to = s.range$max, by=.001)
    d.vals <- fitted.line$intercept + fitted.line$slope * s.vals
    # transform regression line coords to TPR by 1 - FPR coords
    tpr.vals <- invlogit((s.vals + d.vals) / 2)
    fpr.vals <- invlogit((s.vals - d.vals) / 2)
    lines(fpr.vals, tpr.vals, col = lcol, lwd = lweight, lty = lpatern)
    graphics.off()
}

################################################
#   Diagnostic PPV and NPV by Prevalence       #
################################################

compute.ppv <- function(sens, spec, prev) {
  npv <- sens * prev / (sens * prev + (1 - spec) * (1 - prev))
}

compute.npv <- function(sens, spec, prev) {
  ppv <- spec * (1 - prev) / (spec * (1 - prev) + (1 - sens) * prev)
}

plot.ppv.npv.by.prev <- function(diagnostic.data, params) {
  params$measure <- "Sens"
  diagnostic.data.sens <- compute.diag.point.estimates(diagnostic.data, params)
  params$measure <- "Spec"
  diagnostic.data.spec <- compute.diag.point.estimates(diagnostic.data, params)
  params$measure <- "NPV"
  diagnostic.data.npv <- compute.diag.point.estimates(diagnostic.data, params)
  params$measure <- "PPV"
  diagnostic.data.ppv <- compute.diag.point.estimates(diagnostic.data, params)
  
  prev <- ((diagnostic.data@TP + diagnostic.data@FN) / 
              (diagnostic.data@TP + diagnostic.data@FN + diagnostic.data@FP + diagnostic.data@TN))
  prev.min <- min(prev)
  prev.max <- max(prev)
  npv <- diagnostic.data.npv@y
  npv <- diagnostic.transform.f("NPV")$display.scale(npv)
  ppv <- diagnostic.data.ppv@y
  ppv <- diagnostic.transform.f("PPV")$display.scale(ppv)
  
  plot(0:1, 0:1, type="n",main="PPV and NPV by Prevalence", xlab="Prevalence", ylab="")
  points(prev, npv, col=3,)
  points(prev, ppv, col=4)
  legend("right", c("Negative predictive value", "Positive predictive value"), bty="n", col=c(3,4), text.col=c(3,4), pch=c(1,1))

  res.sens <- rma.uni(yi=diagnostic.data.sens@y, sei=diagnostic.data.sens@SE, 
                     slab=diagnostic.data.sens@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)
  res.spec <- rma.uni(yi=diagnostic.data.spec@y, sei=diagnostic.data.spec@SE, 
                     slab=diagnostic.data.spec@study.names,
                     method="FE", level=params$conf.level,
                     digits=params$digits)                     
  sens.est <- diagnostic.transform.f("Sens")$display.scale(res.sens$b[1])
  spec.est <- diagnostic.transform.f("Spec")$display.scale(res.spec$b[1])
  prev.overall <- seq(from=prev.min, to=prev.max, by=.01)
  sens.overall <- rep(sens.est, length(prev.overall))
  spec.overall <- rep(spec.est, length(prev.overall))
  npv.overall <- compute.npv(sens.overall, spec.overall, prev.overall)
  ppv.overall <- compute.ppv(sens.overall, spec.overall, prev.overall)
  lines(prev.overall, npv.overall, col=3)
  lines(prev.overall, ppv.overall, col=4)
  
 
}
#######################################################
#  Functions for formatting data for display in plots #
#######################################################

format.data.cols <- function(plot.data) {
  # formats data columns for display on forest plot
  options <- plot.data$options
  types <- plot.data$types
  if (options$show.col2==TRUE) {
    
        y.disp <- plot.data$effects.disp$y.disp
        lb.disp <- plot.data$effects.disp$lb.disp
        ub.disp <- plot.data$effects.disp$ub.disp
        effect.sizes <- format.effect.sizes(y=y.disp, lb=lb.disp, ub=ub.disp, options)
        # first row contains headers, so add label
        effect.size.label <- create.effect.size.label(effect.sizes, options)
        effect.size.col <- c(effect.size.label,
                             paste(effect.sizes$y.display, effect.sizes$lb.display, ",", 
                                   effect.sizes$ub.display, ")", sep = ""))
        plot.data$additional.col.data$es <- effect.size.col
  }  
  if ((options$show.col3==TRUE) && (!is.null(plot.data$col3))) {
        label <- options$col3.str
        data.col <- format.raw.data.col(nums = plot.data$col3$nums, denoms = plot.data$col3$denoms, label = label, types=types) 
        plot.data$additional.col.data$cases = data.col
  }
  if ((options$show.col4==TRUE) && (!is.null(plot.data$col4))) {
        label <- options$col4.str
        data.col <- format.raw.data.col(nums = plot.data$col4$nums, denoms = plot.data$col4$denoms, label = label, types=types) 
        plot.data$additional.col.data$controls = data.col
  }
  plot.data
}

format.effect.sizes <- function(y, lb, ub, options) {
  # format column by padding entries with spaces for alignment
  digits <- options$digits
  y.display <- round(y, digits)
  y.display <- sprintf(paste("%.", digits,"f", sep=""), y.display)
  lb.display <- round(lb, digits)
  lb.display <- sprintf(paste("%.", digits,"f", sep=""), lb.display)
  ub.display <- round(ub, digits)
  ub.display <- sprintf(paste("%.", digits,"f", sep=""), ub.display)
                       
  # for ub, add an extra space to positive numbers for alignment (negative numbers display minus sign)
  if (length(ub.display[ub.display >= 0])) {
    ub.display[ub.display >= 0] <- mapply(pad.with.spaces, ub.display[ub.display >= 0], begin.num=1, end.num=0)
  }
  # format results by padding with spaces to align columns 
  ub.max.chars <- max(nchar(ub.display))
  ub.extra.space <- ub.max.chars - nchar(ub.display)
  ub.display <- mapply(pad.with.spaces, ub.display, begin.num = ub.extra.space, end.num=0)
  # for ub, add an extra space to positive numbers for alignment (negative numbers display minus sign)
  if (length(ub.display[ub.display >= 0])) {
    ub.display[ub.display >= 0] <- mapply(pad.with.spaces, ub.display[ub.display >= 0], begin.num=1, end.num=0)
  }
  # if ub has any negative entries, add an extra space to separate entry from preceding ","
  if (min(ub) < 0) {
    ub.display <- paste(" ", ub.display, sep="")
  }
  lb.display <- paste(" (", lb.display, sep="")
  lb.max.chars <- max(nchar(lb.display))
  lb.extra.space <- lb.max.chars - nchar(lb.display)
  lb.display <- mapply(pad.with.spaces, lb.display, begin.num = lb.extra.space, end.num=0)
  effect.sizes <- list("y.display"=y.display, "lb.display"=lb.display, "ub.display"=ub.display)
}

create.effect.size.label <- function(effect.sizes, options) {
   # add label to effect.size.column and align
   # The purpose of this code is to align the comma if the label
   # is of the form ES(LL, UL), with the data entries below it. Since the default label
   # is no longer of that form, this function could be removed.
   col2.label <- as.character(options$col2.str)
   # if label contains ",", pad label to align columns
   label.info <- check.label(label = col2.label, split.str = ",")
   max.chars <- max(nchar(effect.sizes$ub.display)) + 1
   # add 1 because a space is added before each ub entry.
   if (label.info$contains.symbol == TRUE) {
     # label contains "," so pad label to align ","
     # we're assuming that there is a single space after ","
     col2.label.padded <- pad.with.spaces(col2.label, begin.num=0, end.num = max.chars - label.info$end.string.length) 
   } else {
     # label doesn't contain "," so pad label to center over column 
     col2.width <- max(nchar(effect.sizes$y.disp) + nchar(effect.sizes$lb.disp) + nchar(effect.sizes$ub.disp))
     if (col2.width > nchar(col2.label)) {
       # width of data greater than the length of col. label 
       col2.label.padded <- pad.with.spaces(col2.label, begin.num=0, end.num = floor((col2.width - nchar(col2.label)) / 2)) 
     } else {
       # don't pad with spaces
       col2.label.padded <- col2.label
     }
   }
   col2.label.padded
}
  
format.raw.data.col <- function(nums, denoms, label, types) {
    # format raw data columns to align forward slashes
    types.short <- types[types %in% c(0,1)]
    # remove types 3 (labels) and 2 (overall total) if present
    nums.total <- sum(nums[types.short==0])
    denoms.total <- sum(denoms[types.short==0])
    # only sum over types==0 (individual studies)
    max.chars <- nchar(denoms.total) + 1
    # add 1 because a space is added before each denom.
    overall.row <- paste(nums.total, " / ", denoms.total, sep = "")
    label.info <- check.label(label, split.str = "/")
    if (label.info$contains.symbol == TRUE) {
        # pad label or denoms.total to align "/"
        # we're assuming that there is a single space after "/".
        end.string.length <- label.info$end.string.length
        label.padded <- pad.with.spaces(label, begin.num=0, end.num = max.chars - end.string.length)
        overall.row <- pad.with.spaces(overall.row, begin.num=0, end.num = end.string.length - max.chars)
        max.chars <- max(max.chars, end.string.length)    
    }  else {
      # pad label to center above column
      label.padded <- pad.with.spaces(label, begin.num=0, end.num = floor((nchar(overall.row) - nchar(label)) / 2))
    }
    # pad data row to align forward slashes
    denoms <- mapply(pad.with.spaces, denoms, begin.num=0, end.num = max.chars - (nchar(denoms) + 1))
    # add 1 to nchar(denoms) because a space is added before each denom
    data.column = c(label.padded, paste(nums, " / ", denoms, sep = ""), overall.row)
    data.column
}

check.label <- function(label, split.str) {
    # utility for format.effect.size.col and format.raw.data.col
    # check column labels for split.symbol and return length of string that follows split.str
    split.label <- strsplit(label, split.str)
    split.label.length <- length(split.label[[1]])
    label.info <- list("contains.symbol"=FALSE, "end.string.length"=0)
    if (split.label.length > 1) {
       label.info$contains.symbol <- TRUE
       label.info$end.string.length <- nchar(split.label[[1]][split.label.length])
    }
    label.info
}

calculate.radii <- function(inv.var, max.symbol.size, max.ratio) {
    # calculates radii of symbols for a meta-regression plot
    # using a scaling function f(x) = C * x^e.
    # inv.var is a vector of inverse variances,
    # max.symbol.size is the maximum size for a plot, and max.ratio is the maximum ratio of symbol sizes.

    inv.var.max <- max(inv.var)
    inv.var.min <- min(inv.var)
    if ((inv.var.max / inv.var.min) <= max.ratio) {
        C <- max.symbol.size / inv.var.max
        exponent <- 1
    } else {
        min.symbol.size <- max.symbol.size / max.ratio
        exponent <- log(max.ratio)/log(inv.var.max / inv.var.min)
        C <- max.symbol.size / (inv.var.max)^exponent
    }
    radii <- C * inv.var^exponent
}     
    
     