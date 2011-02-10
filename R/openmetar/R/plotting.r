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
    
    if (params$fp_show_col2=='TRUE') {
        # transform entries to display scale
        y.trans <- eval(call(transform.name, params$measure))$display.scale(y)
        lb.trans <- eval(call(transform.name, params$measure))$display.scale(lb)
        ub.trans <- eval(call(transform.name, params$measure))$display.scale(ub)
        y.overall.trans <- eval(call(transform.name, params$measure))$display.scale(y.overall)
        lb.overall.trans <- eval(call(transform.name, params$measure))$display.scale(lb.overall)
        ub.overall.trans <- eval(call(transform.name, params$measure))$display.scale(ub.overall)
        
        y.display <- round.with.zeros(y.trans, digits = params$digits)
        lb.display <- round.with.zeros(lb.trans, digits = params$digits)
        ub.display <- round.with.zeros(ub.trans, digits = params$digits)
        y.overall.display <- round.with.zeros(y.overall.trans, digits = params$digits)
        lb.overall.display <- round.with.zeros(lb.overall.trans, digits = params$digits)
        ub.overall.display <- round.with.zeros(ub.overall.trans, digits = params$digits)
        
        # for ub, ub.overall, add an extra space to positive numbers for alignment (negative numbers display minus sign)
        if (length(ub.display[ub.display >= 0])) {
            ub.display[ub.display >= 0] <- mapply(pad.with.spaces, ub.display[ub.display >= 0], begin.num=1, end.num=0)
        }
        if (length(ub.overall.display[ub.overall.display >= 0])) {
            ub.overall.display[ub.overall.display >= 0] <- mapply(pad.with.spaces, ub.overall.display[ub.overall.display >= 0], begin.num=1, end.num=0)
        }
        # if ub, ub.overall have any negative entries, add an extra space to separate entry from preceding ","
        if (min(ub.trans) < 0) {
            ub.display <- paste(" ", ub.display, sep="")
        }
        if (min(ub.overall.trans) < 0) {
            # if any negative entries, add an extra space to separate entry from preceding ","
            ub.overall.display <- paste(" ", ub.overall.display, sep="")
        }
        
        # format results by padding with spaces to align columns 
        ub.max.chars <- max(nchar(ub.display), nchar(ub.overall.display))
        ub.extra.space <- ub.max.chars - nchar(ub.display)
        ub.overall.extra.space <- ub.max.chars - nchar(ub.overall.display)
        ub.display <- mapply(pad.with.spaces, ub.display, begin.num = ub.extra.space, end.num=0)
        ub.overall.display <- pad.with.spaces(ub.overall.display, begin.num = ub.overall.extra.space, end.num=0)
    
        lb.display <- paste(" (", lb.display, sep="")
        lb.overall.display <- paste(" (", lb.overall.display, sep="")
        lb.max.chars <- max(nchar(lb.display), nchar(lb.overall.display))
        lb.extra.space <- lb.max.chars - nchar(lb.display)
        lb.overall.extra.space <- lb.max.chars - nchar(lb.overall.display)
        lb.display <- mapply(pad.with.spaces, lb.display, begin.num = lb.extra.space, end.num=0)
        lb.overall.display <- pad.with.spaces(lb.overall.display, begin.num = lb.overall.extra.space, end.num=0)

        col2.overall.row <- paste(y.overall.display, lb.overall.display, ",", ub.overall.display, ")", sep = "")
        col2.width <- nchar(col2.overall.row)
        col2.label <- as.character(params$fp_col2_str)
        # if label contains ",", pad label to align columns
        label.info <- check.label(label = col2.label, split.str = ",")
        max.chars <- max(nchar(ub.display), nchar(ub.overall.display)) + 1
        # add 1 because a space is added before each ub entry.
        if (label.info$contains.symbol == TRUE) {
            # label contains "," so pad label to align ","
            # we're assuming that there is a single space after ","
            col2.label.padded <- pad.with.spaces(col2.label, begin.num=0, end.num = max.chars - label.info$end.string.length) 
        } else {
            # label doesn't contain "," so pad label to center over column 
            col2.label.padded <- pad.with.spaces(col2.label, begin.num=0, end.num = floor((col2.width - nchar(col2.label)) / 2))           
        }
        additional.cols <- list(es = c(col2.label.padded,
                                 paste(y.display, lb.display, ",", ub.display, ")", sep = ""),
                                 col2.overall.row))
        plot.data$additional.col.data$es <- c(col2.label.padded,
                                 paste(y.display, lb.display, ",", ub.display, ")", sep = ""),
                                 col2.overall.row)
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
        data.column <- format.raw.data.col(nums = binary.data@g1O1, denoms = binary.data@g1O1 + binary.data@g1O2, label = as.character(params$fp_col3_str))
        plot.data$additional.col.data$cases <- data.column
    }
    
    if ((length(binary.data@g2O1) > 0) && (params$fp_show_col4=="TRUE")){
        data.column <- format.raw.data.col(nums = binary.data@g2O1, denoms = binary.data@g2O1 + binary.data@g2O2, label = as.character(params$fp_col4_str))
        plot.data$additional.col.data$controls = data.column
    }
    
    plot.data
}

create.plot.data.diagnostic <- function(diagnostic.data, params, res, selected.cov = NULL, include.overall=TRUE){

    plot.data <- create.plot.data.generic(diagnostic.data, params, res, selected.cov=selected.cov)
        
    # if we have raw data, add it to the additional columns field
    if ((length(diagnostic.data@TP) > 0) && (params$fp_show_col3=="TRUE")) {
        metric <- params$measure
        label <- switch(metric,
        # sensitivity
        Sens = "TP/Di+", 
        # specificity
        Spec = "TN/D-",
        # pos. predictive value
        PPV =  "TP/T+",
        #neg. predictive value
        NPV =  "TN/T-",
        # accuracy
        Acc = "(TP + TN)/Tot",
        # positive likelihood ratio
        PLR = "Sens/(1-Spec)", 
        # negative likelihood ratio
        NLR = "(1-Sens)/Spec",
        # diagnostic odds ratio
        DOR = "(TP * TN)/(FP * FN)")
        
        data.col <- format.raw.data.col(nums = diagnostic.data@numerator, denoms = diagnostic.data@denominator, label = label) 
        plot.data$additional.col.data$cases = data.col
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
   
    if (params$fp_show_col2=='TRUE') {
        # put results in display scale and round.
        y.display <- binary.transform.f(params$measure)$display.scale(y)
        lb.display <- binary.transform.f(params$measure)$display.scale(lb)
        ub.display <- binary.transform.f(params$measure)$display.scale(ub)
        y.display <- round.with.zeros(y.display, digits = params$digits)
        lb.display <- round.with.zeros(lb.display, digits = params$digits)
        lb.display[lb.display >= 0] <- pad.with.spaces(lb.display[lb.display >= 0], begin.num=0, end.num=1)
        ub.display <- round.with.zeros(ub.display, digits = params$digits)
        ub.display[ub.display >= 0] <- pad.with.spaces(ub.display[ub.display >= 0], begin.num=1, end.num=0)
        additional.cols <- list(es = c(paste(params$fp_col2_str, sep = ""),
                                 paste(y.display, " (", lb.display, " , ", ub.display, ")", sep = "")))
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
     se <- reg.data@SE
     if (!is.null(selected.cov)){
        cov.val.str <- paste("reg.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
     } 
     effects <- list(ES = y,
                    se = se)
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
                      x=1, just = "right", gp = gpar(fontface = font, fontfamily="mono")))
          else
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], 
                      x=1, just = "right", gp = gpar(fontface = "plain", fontfamily="mono")))
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
    # i have kept the "ifs" below: when we decide to include more metrics
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
                 }   else  { # if no additional colums things are simple
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

    # Draw the text in study col and additional cols
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

diagnostic.sroc.plot <- function(plot.data, outpath,
                                  symSize=1,
                                  lcol = "darkred",
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen") {


    # make the data data.frame
    #data.reg <- data.frame(plot.data$effects, types = plot.data$types)
    # data for plot (only keep the studies - not the summaries)
    #data.reg <- subset(data.reg, types==0)
    fitted.line <- plot.data$fitted.line
    weighted <- plot.data$weighted
    TPR <- plot.data$TPR
    FPR <- plot.data$FPR
    s.range <- plot.data$s.range
    
    png(file=outpath, width=5 , height=5, units="in", res=144)
    if (weighted == TRUE) {
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

#######################################################
#   Functions for formatting data for display in plots #
#######################################################
  
check.label <- function(label, split.str) {
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

format.raw.data.col <- function(nums, denoms, label) {
    # format raw data columns to align forward slashes
    nums.total <- sum(nums)
    denoms.total <- sum(denoms)
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

calculate.radii <- function(inv.var, max.symbol.size, max.ratio) {
    # calculates radii of symbols for a meta-regression plot
    # using a scaling function f(x) = C * x^e.
    # inv.var is a vector of inverse variances,
    # max.symbol.size is the maximum size for a plot, and max.ratio is the maximum ratio of symbol sizes.

    inv.var.max <- max(inv.var)
    inv.var.min <- min(inv.var)
    if ((inv.var.max / inv.var.min) <= max.ratio) {
        C <- max.symbol.size / inv.var.max
        e <- 1
    } else {
        min.symbol.size <- max.symbol.size / max.ratio
        exponent <- log(max.ratio)/log(inv.var.max / inv.var.min)
        C <- max.symbol.size / (inv.var.max)^e
    }
    radii <- C * inv.var^exponent
}     
    
     