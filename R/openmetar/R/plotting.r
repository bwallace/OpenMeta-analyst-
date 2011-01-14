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
#   Dahabreh                       #    
####################################

# largely a generalization based on an example by
# Murrell P., "R graphics", Chapman & Hall

library("grid")

#####################################################
#   functions for data manipulation and forest plot #
#####################################################

create.plot.data.generic <- function(om.data, params, res, selected.cov=NULL){
    scale.str <- "cont"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    }
    if (metric.is.logit.scale(params$measure)){
        scale.str <- "logit"
    }
    if ("DiagnosticData" %in% class(om.data)) {
        transform.name <- "diagnostic.transform.f"
    }  else {
        # data is binary or cont
        transform.name <- "binary.transform.f"
    }
    
    
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    ## TODO note that we're forcing the 'cont' scale -- thus
    # it's assumed everything is on the raw scale. may want to change
    # this.
    plot.data <- list(label = c(paste(params$fp_col1_str, sep = ""), om.data@study.names, "Overall"),
                    types = c(3, rep(0, length(om.data@study.names)), 2),
                    scale = scale.str)
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    y.overall <- res$b[1]
    lb.overall <- res$ci.lb[1]
    ub.overall <- res$ci.ub[1]
    ###
    # TODO we're making tacit assumptions here
    # about the subclass of OmData; namely
    # that it includes a y and SE field.
    #
    # put results in display scale and round.
    y <- om.data@y
    lb <- y - mult*om.data@SE
    ub <- y + mult*om.data@SE
  
    y.display <- eval(call(transform.name, params$measure))$display.scale(y)
    lb.display <- eval(call(transform.name, params$measure))$display.scale(lb)
    ub.display <- eval(call(transform.name, params$measure))$display.scale(ub)
    y.overall.display <- eval(call(transform.name, params$measure))$display.scale(y.overall)
    lb.overall.display <- eval(call(transform.name, params$measure))$display.scale(lb.overall)
    ub.overall.display <- eval(call(transform.name, params$measure))$display.scale(ub.overall)
    
    # some formatting to align columns - pad with zeros and add extra space to positive numbers
    y.rounded <- round.with.zeros(y.display, digits = params$digits)
    lb.rounded <- round.with.zeros(lb.display, digits = params$digits)
    lb.rounded[lb.rounded >= 0] <- pad.with.spaces(lb.rounded[lb.rounded >= 0], begin.num=0, end.num=1)
    ub.rounded <- round.with.zeros(ub.display, digits = params$digits)
    ub.rounded[ub.rounded >= 0] <- pad.with.spaces(ub.rounded[ub.rounded >= 0], begin.num=1, end.num=0)
    y.overall.rounded <- round.with.zeros(y.overall.display, params$digits)
    lb.overall.rounded <- round.with.zeros(lb.overall.display, params$digits)
    lb.overall.rounded[lb.overall.rounded >= 0] <- pad.with.spaces(lb.overall.rounded[lb.overall.rounded >= 0], begin.num=0, end.num=1)
    ub.overall.rounded <- round.with.zeros(ub.overall.display, params$digits)
    ub.overall.rounded[ub.overall.rounded >= 0] <- pad.with.spaces(ub.overall.rounded[ub.overall.rounded >= 0], begin.num=1, end.num=0)
    
        
    
    if (params$fp_show_col2=='TRUE') {
        col2.overall.row <- paste(y.overall.rounded, " (", lb.overall.rounded, " , ", ub.overall.rounded, ")", sep = "")
        col2.width <- nchar(col2.overall.row)
        col2.label <- params$fp_col2_str
        label2.width <- nchar(col2.label)
        col2.label.padded <- pad.with.spaces(col2.label, begin.num=0, end.num = floor((col2.width - label2.width) / 2))
        additional.cols <- list(es = c(col2.label.padded,
                                 paste(y.rounded, " (", lb.rounded, " , ", ub.rounded, ")", sep = ""),
                                 col2.overall.row))
        plot.data$additional.col.data <- additional.cols 
    }              
    effects <- list(ES = c(y, y.overall),
                    LL = c(lb, lb.overall),
                    UL = c(ub, ub.overall))
    plot.data$effects <- effects
    # covariates
    if (!is.null(selected.cov)){
        cov.val.str <- paste("om.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }
    plot.data$fp_xlabel <- paste(params$fp_xlabel, sep = "")
    plot.data$fp_xticks <- params$fp_xticks
    plot.data
}


metric.is.log.scale <- function(metric){
    metric %in% c(binary.log.metrics, diagnostic.log.metrics)    
}

metric.is.logit.scale <- function(metric) {
    metric %in% c(diagnostic.logit.metrics)
}    

create.plot.data.binary <- function(binary.data, params, res, selected.cov = NULL, include.overall=TRUE){

    plot.data <- create.plot.data.generic(binary.data, params, res, selected.cov=selected.cov)
        
    # if we have raw data, add it to the additional columns field
    if ((length(binary.data@g1O1) > 0) && (params$fp_show_col3=="TRUE")) {
        # pad the right side of data with spaces to allign /
        col3.denoms <- binary.data@g1O1 + binary.data@g1O2
        col3.denom.digits <- nchar(col3.denoms)
        col3.total <- sum(binary.data@g1O1 + binary.data@g1O2)
        col3.max.digits <- nchar(col3.total)
        col3.denoms.padded <- mapply(pad.with.spaces, col3.denoms, begin.num=0, end.num = 2 * (col3.max.digits - col3.denom.digits))
        col3.overall.row <- paste(sum(binary.data@g1O1), " / ", col3.total, sep = "")
        col3.width <- nchar(col3.overall.row)
        col3.label <- params$fp_col3_str
        label3.width <- nchar(col3.label)
        # pad the column label to center it over data
        col3.label.padded <- pad.with.spaces(col3.label, begin.num=0, end.num = floor((col3.width - label3.width) / 2))
        plot.data$additional.col.data$cases = c(col3.label.padded, 
                                    paste(binary.data@g1O1, " / ", col3.denoms.padded, sep = ""), 
                                    col3.overall.row)
    }
    
    if ((length(binary.data@g2O1) > 0) && (params$fp_show_col4=="TRUE")){
        col4.denoms <- binary.data@g2O1 + binary.data@g2O2
        col4.denom.digits <- nchar(col4.denoms)
        col4.total <- sum(binary.data@g2O1 + binary.data@g2O2)
        col4.max.digits <- nchar(col4.total)
        col4.denoms.padded <- mapply(pad.with.spaces, col4.denoms, begin.num=0, end.num = 2 * (col4.max.digits - col4.denom.digits))
        col4.overall.row <- paste(sum(binary.data@g2O1), " / ", col4.total, sep = "")
        col4.width <- nchar(col4.overall.row)
        col4.label <- params$fp_col4_str
        label4.width <- nchar(col4.label)
        col4.label.padded <- pad.with.spaces(col4.label, begin.num=0, end.num = floor((col4.width - label4.width) / 2))
        plot.data$additional.col.data$controls = c(col4.label.padded,
                                        paste(binary.data@g2O1, " / ", col4.denoms.padded, sep = ""),
                                        col3.overall.row)
    }
    
    plot.data
}

create.plot.data.diagnostic <- function(diagnostic.data, params, res, selected.cov = NULL, include.overall=TRUE){

    plot.data <- create.plot.data.generic(diagnostic.data, params, res, selected.cov=selected.cov)
        
    # if we have raw data, add it to the additional columns field
    if ((length(diagnostic.data@TP) > 0) && (params$fp_show_col3=="TRUE")) {
        plot.data$additional.col.data$cases = c(paste(params$fp_col3_str, sep = ""), 
                                    paste(diagnostic.data@TP, " / ", diagnostic.data@TP, " + ", diagnostic.data@FN, sep = ""), 
                                    paste(sum(diagnostic.data@TP), " / ", sum(diagnostic.data@TP + diagnostic.data@FN), sep = ""))
    }
    
    if ((length(diagnostic.data@TN) > 0) && (params$fp_show_col4=="TRUE")){
        plot.data$additional.col.data$controls = c(paste(params$fp_col4_str, sep = ""),
                                        paste(diagnostic.data@TN, " / ", diagnostic.data@TN, " + ", diagnostic.data@FP, sep = ""),
                                        paste(sum(diagnostic.data@TN), " / ", sum(diagnostic.data@TN + diagnostic.data@FP), sep = ""))
    }
    
    plot.data
}

create.plot.data.continuous <- function(cont.data, params, res, selected.cov = NULL, include.overall=TRUE){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    plot.data <- create.plot.data.generic  (cont.data, params, res, selected.cov=selected.cov)
    
    plot.data
}

create.plot.data.overall <- function(params, res, study.names, addRow1Space, selected.cov=NULL){
    scale.str <- "cont"
    # Add space to row 1 for cumulative ma to align study names.
    if (addRow1Space == TRUE) {
        study.names[1] <- paste("   ", study.names[1], sep="")
    }
    plot.data <- list( label = c("Studies", study.names),
                types = c(3, rep(0, length(study.names))),
                scale = scale.str)
    y <- res[,1]
    lb <- res[,2]
    ub <- res[,3]
   
    # put results in display scale and round.
    y.display <- binary.transform.f(params$measure)$display.scale(y)
    lb.display <- binary.transform.f(params$measure)$display.scale(lb)
    ub.display <- binary.transform.f(params$measure)$display.scale(ub)
    
    y.rounded <- round.with.zeros(y.display, digits = params$digits)
    lb.rounded <- round.with.zeros(lb.display, digits = params$digits)
    lb.rounded[lb.rounded >= 0] <- pad.with.spaces(lb.rounded[lb.rounded >= 0], begin.num=0, end.num=1)
    ub.rounded <- round.with.zeros(ub.display, digits = params$digits)
    ub.rounded[ub.rounded >= 0] <- pad.with.spaces(ub.rounded[ub.rounded >= 0], begin.num=1, end.num=0)
    
    if (params$fp_show_col2=='TRUE') {
        additional.cols <- list(es = c(paste(params$fp_col2_str, sep = ""),
                                 paste(y.rounded, " (", lb.rounded, " , ", ub.rounded, ")", sep = "")))
        plot.data$additional.col.data <- additional.cols 
    }      
      
    effects <- list(ES = y,
                    LL = lb,
                    UL = ub)
    plot.data$effects <- effects
    plot.data
}

# create regression plot data
create.plot.data.reg <- function(reg.data, params, fitted.line, selected.cov=cov.name) {
     scale.str <- "cont"
     plot.data <- list("fitted.line" = fitted.line,
                    types = c(rep(0, length(reg.data@study.names))),
                    scale = scale.str)
     alpha <- 1.0-(params$conf.level/100.0)
     mult <- abs(qnorm(alpha/2.0))
    
     y <- reg.data@y
     lb <- y - mult*reg.data@SE
     ub <- y + mult*reg.data@SE               
     if (!is.null(selected.cov)){
        cov.val.str <- paste("reg.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
     } 
     effects <- list(ES = y,
                    LL = lb,
                    UL = ub)
     plot.data$effects <- effects
     plot.data
}

# get data for the study column
study.column <- function(forest.data, title.font="bold") {
    content<-rep(NA, length(forest.data$label))
    for (i in 1:length(forest.data$label)){
      if (forest.data$types[i] !=  0)
        content[i] <- list(textGrob(forest.data$label[i], x=0, just = "left", gp = gpar(fontface = title.font)))
      else
        content[i] <- list(textGrob(forest.data$label[i], x=0, just = "left", gp = gpar(fontface = "plain")))
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
                      x=1, just = "right", gp = gpar(fontface = font)))
          else
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], 
                      x=1, just = "right", gp = gpar(fontface = "plain")))
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


effectsize.column <- function(forest.data) {
    
    rows<-c(1, rep(NA, (length(forest.data$label)-1) ) )
    for (i in 1:(length(forest.data$label) -1)){
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
    
    list(ES = forest.data$effects$ES, LL = forest.data$effects$LL, 
                  UL = forest.data$effects$UL, rows = rows[-1], types = forest.data$types[-1])
}

plot.options <- function(forest.data, box.sca = 1, gapSize=3, plotWidth = 4) {
    # weights for the boxes
    # note that 1.96 is a convention [not necessary for the scalling]
    # the analysis functions determine the CI width (e.g. 95% or 99%)
    # this is just scaling the boxes according to the SE
    precision <- NULL
    effect.col.range <- NULL
    effect.col<-forest.data$effects
    # i have kept the "ifs" bellow: when we decide to include more metrics
    # these will be expanded
    
    if (forest.data$scale == "log"){
           precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    } else if (forest.data$scale == "cont") {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    } else {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    }
      
   if (forest.data$scale != "diagnostic") { 
   # some heuristics to determine xscale (unnecessary for diagnostic data) 
        if (min(effect.col$LL)>0 && max(effect.col$UL)>0) {
                effect.col.range <- c(max(0.5*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
        } else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)<=0) { 
                effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(-1.5*max(effect.col$ES) , max(effect.col$UL)))
        } else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)>0) { 
                effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
        } else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)>0 && max(effect.col$ES)>0) { 
                effect.col.range <- c(max(-2*min(effect.col$ES) , min(effect.col$LL)), min(1.5*max(effect.col$ES) , max(effect.col$UL)))
        } else if (min(effect.col$LL)<=0 && max(effect.col$UL)<0) { 
                effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(0.10*max(effect.col$ES) , max(effect.col$UL)))
        }                                       
   
        # this is an ugly solution to an uncommon problem                                 
        merge.data <- data.frame(x = forest.data$types[-1], y = effect.col$LL, z = effect.col$UL)
        merge.data <- subset(merge.data, x>0)                                  
    
        if (min(effect.col.range) >= min(merge.data$y)) 
           effect.col.range[1] <- min(merge.data$y)
        if (max(effect.col.range) <= max(merge.data$z)) 
           effect.col.range[2] <- max(merge.data$z)
   }
     
   else {
   # if data is diagnostic, effect.col.range is [0, 1]
      effect.col.range <- c(0, 1)
   }    
            
   effect.col.sizes <- box.sca * precision/max(precision)
   effect.col.width <- unit(plotWidth, "inches")

    forest.params = list(
        col.gap = unit(gapSize, "mm"),
        precision = precision,
        effect.col.range = effect.col.range,
        effect.col.sizes = effect.col.sizes,
        effect.col.width = effect.col.width
    )
    forest.params
}

# Function to draw a cell in a text column
draw.label.col <- function(col, j) {
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
  if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) > 0){
    grid.arrows(x=unit(c(LL, 1), c("native", "npc")), length=unit(0.05, "inches"))                 
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) < 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0){
    grid.arrows(x=unit(c(UL, 0), c("native", "npc")), length=unit(0.05, "inches"))
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1   &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0 ){
    grid.arrows(x=unit(c(ES, 0), c("native", "npc")), length=unit(0.05, "inches")) 
    grid.arrows(x=unit(c(ES, 1), c("native", "npc")), length=unit(0.05, "inches"))              
  }
  else {
    # Draw line white if totally inside rect
    line.col <- if ((convertX(unit(ES, "native") + unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) > UL) &&
                   (convertX(unit(ES, "native") - unit(0.5*size, "lines"),
                             "native", valueOnly=TRUE) < LL))
      "white"
    else
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

# Function to draw a "forest" column
draw.data.col <- function(forest.data, col, j, color.overall = "black",
                          color.subgroup = "black", summary.line.col = "darkred",
                          summary.line.pat = "dashed",
                          metric = "Effect size", diam.size=1,
                          user.ticks = NULL) {
                          
  pushViewport(viewport(layout.pos.col=j, xscale=col$range))
  
# This is the "null" line
# "ifs" left in as we will possibly expand this when new metrics become available
# note that the line is to be supressed when out of xscale bounds
  if (forest.data$scale == "log" && min(col$range)<0 && max(col$range)>0 ) {
      grid.lines(x=unit(0, "native"), y=0:1)
  }
  if (forest.data$scale == "cont" && min(col$range)<0 && max(col$range)>0 ) { 
      grid.lines(x=unit(0, "native"), y=0:1)
  }
  if (forest.data$scale == "logit" && min(col$range)<0 && max(col$range)>0 ) { 
      grid.lines(x=unit(0, "native"), y=0:1)
  }
  # Assume that last value in col is "All" 
  grid.lines(x=unit(col$ES[length(col$ES)], "native"),
             y=0:1, gp=gpar(lty = summary.line.pat, col= summary.line.col))
  
  if  (forest.data$scale == "cont") {
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
		        log.ticks <- log(user.ticks)
		        #log.ticks <- log.ticks[log.ticks > min(col$range) - 0.5]    # remember it is additive on this scale
            #log.ticks <- log.ticks[log.ticks < max(col$range) + 0.5]
            ticks <- exp(log.ticks)
        }
        
        grid.xaxis(at = log.ticks , label = round(ticks, 3), gp=gpar(cex=0.6))          
  } 
  if (forest.data$scale == "logit") {
        if (length(user.ticks) == 0) { # some cheap tricks to make the axis ticks look nice (in most cases)...
            logit.ticks <- floor(col$range[1]):ceiling(col$range[2])
            ticks <- invlogit(logit.ticks)
        } else {
		        logit.ticks <- logit(user.ticks)
		        #log.ticks <- log.ticks[log.ticks > min(col$range) - 0.5]    # remember it is additive on this scale
            #log.ticks <- log.ticks[log.ticks < max(col$range) + 0.5]
            ticks <- invlogit(logit.ticks)
        }
        
        grid.xaxis(at = logit.ticks , label = round(ticks, 3), gp=gpar(cex=0.6)) 
  }
       
  grid.text(metric, y=unit(-2, "lines"))
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
forest.plot <- function(forest.data, outpath){
    # these are calls to data functions
    study.col <- study.column(forest.data, "bold")
    additional.cols <- c()
    if (length(forest.data$additional.col.data)>0 ){
        additional.cols <- additional.columns(forest.data, "bold")    
    } 
    
    effects.col <- effectsize.column(forest.data)
    forest.plot.params <- plot.options(forest.data, box.sca=0.8, gapSize = 3.2, plotWidth=5)

    # these are calls to plotting functions
    extra.space <- sum(forest.data$types != 0) 
    height <- length(forest.data$types)+ extra.space
  
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
        how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
                        "grobwidth", study.col$content)), "inches" , valueOnly=TRUE  ) +
        convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )  +
                 sum( convertX(   unit.c(width.list[[length(additional.cols)]]) , "inches" , valueOnly=TRUE ) )  + 
                 length(additional.cols)*   convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )   + 
                 convertX(forest.plot.params$effect.col.width, "inches" , valueOnly=TRUE ) 
        how.tall <- convertY(unit(rep(1, height)  , "lines"), "inches" , valueOnly=TRUE )
        png(file=outpath, width = how.wide + 1, height = height*how.tall+2 , units = "in", res = 144)
        pushViewport(viewport(layout=grid.layout(height ,2*length(additional.cols)+3,
                 width=
                     unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                         forest.plot.params$col.gap,  width.list[[length(additional.cols)]]  ,  forest.plot.params$effect.col.width),
                 height = unit(rep(1, height)  , "lines"))))
                 }   else  { # if no additional colums thins are simple
                 how.wide <- convertX(max(unit(rep(1, length(forest.data$label)), 
                                 "grobwidth", study.col$content)), "inches" , valueOnly=TRUE  ) +
                                 convertX(forest.plot.params$col.gap, "inches" , valueOnly=TRUE )  +
                                 convertX(forest.plot.params$effect.col.width, "inches" , valueOnly=TRUE ) 
                 how.tall <- convertY(unit(rep(1, height)  , "lines") , "inches" , valueOnly=TRUE ) 
                 png(file=outpath, width = how.wide + 1, height = height*how.tall+2 , units = "in", res = 144)                      
                 pushViewport(viewport(layout=grid.layout(height ,2*length(additional.cols)+3,
                 width=
                     unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                     forest.plot.params$col.gap,   forest.plot.params$effect.col.width),
                 height = unit(rep(1, height)  , "lines"))))
                }
    #number.cols <- 2 + length(additional.cols)
    draw.label.col(study.col, 1)
    if (length(additional.cols)>0 )  {
           for (i in 1:length(additional.cols)){
               draw.label.col(additional.cols[[i]], 1+2*i)
        }
    }  
    # this could (ahem, should) be better refactored
    # we pull out some values for the effects column that
    # are computed in the plot.params method -- but they're
    # not really 'plot parameters'. they should be computed
    # elsewhere.
    effects.col$range <- forest.plot.params$effect.col.range
    effects.col$sizes <- forest.plot.params$effect.col.sizes
    effects.col$width <- forest.plot.params$effect.col.width

    xticks <- forest.data$fp_xticks
    if (!is.null(xticks)) {
        if (xticks == "NULL"){
            xticks <- NULL
        }
        else{
            xticks <- eval(parse(text=paste("c(", xticks, ")", sep="")))
        }
    }
    effect.size.str <- c(paste(forest.data$fp_xlabel, sep=""))
  
    draw.data.col(forest.data, effects.col, 2*length(additional.cols)+3,
                             color.overall = "lightblue",
                             color.subgroup = "yellow",
                             summary.line.col= "red",
                             summary.line.pat = "dashed",
                             metric = effect.size.str,
                             diam.size = 1.2,
                             user.ticks = xticks)
    graphics.off()
}
 

#######################################
#       meta-regression scatter       #
#######################################
meta.regression.plot <- function(plot.data, outpath,
                                  symSize=1,
                                  lcol = "darkred",
                                  metric = "Effect size",
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
    
    # area of circles
    precision = NULL
    mult = 1.96 # again, 1.96 is a convention for scaling, no need to parameterize
    if (plot.data$scale == "log"){
         precision <- 1 / ((data.reg$UL - data.reg$LL)/(2*mult))
    }
    else if (plot.data$scale == "cont"){
        precision <- 1 / ((data.reg$UL - data.reg$LL)/(2*mult))
    }
    
    radii <-  precision/sum(precision)
    # TODO need to do something about the scaling.
    png(file=outpath, width=5 , height=5, units="in", res=144)
    cov.name <- plot.data$covariate$varname
    cov.values <- plot.data$covariate$values
    #depends on whether these are natural or log
    if (plot.data$scale == "cont"){
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches=FALSE,
              xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
    }
    else{ 
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches = FALSE,
              xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
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

diagnostic.SROC.plot <- function(plot.data, outpath,
                                  symSize=1,
                                  lcol = "darkred",
                                  metric = "Effect size",
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
    
    # area of circles
    precision = NULL
    mult = 1.96 # again, 1.96 is a convention for scaling, no need to parameterize
    if (plot.data$scale == "log"){
         precision <- 1 / ((data.reg$UL - data.reg$LL)/(2*mult))
    }
    else if (plot.data$scale == "cont"){
        precision <- 1 / ((data.reg$UL - data.reg$LL)/(2*mult))
    }
    
    radii <-  precision/sum(precision)
    # TODO need to do something about the scaling.
    png(file=outpath, width=5 , height=5, units="in", res=144)
    cov.name <- plot.data$covariate$varname
    cov.values <- plot.data$covariate$values
    #depends on whether these are natural or log
    if (plot.data$scale == "cont"){
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches=FALSE,
              xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
    }
    else{ 
        symbols(y = data.reg$ES, x = cov.values, circles = symSize*radii , inches = FALSE,
              xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
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