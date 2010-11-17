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
    
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    ## TODO note that we're forcing the 'cont' scale -- thus
    # it's assumed everything is on the raw scale. may want to change
    # this.
    plot.data <- list(label = c(paste(params$fp_col1_str, sep = ""), om.data@studyNames, "Overall"),
                    types = c(3, rep(0, length(om.data@studyNames)), 2),
                    scale = scale.str)
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    ###
    # TODO we're making tacit assumptions here
    # about the subclass of OmData; namely
    # that it includes a y and SE field.
    lb <- om.data@y - mult*om.data@SE
    ub <- om.data@y + mult*om.data@SE
    y <- om.data@y
    lb <- binary.transform.f(params$measure)$display.scale(lb)
    ub <- binary.transform.f(params$measure)$display.scale(ub)
    y <- binary.transform.f(params$measure)$display.scale(y)
    y.overall <- binary.transform.f(params$measure)$display.scale(res$b[1])
    lb.overall <- binary.transform.f(params$measure)$display.scale(res$ci.lb[1])
    ub.overall <- binary.transform.f(params$measure)$display.scale(res$ci.ub[1])
    # round results for display.
    y.rounded <- roundWithZeros(y, params$digits)
    lb.rounded <- roundWithZeros(lb, params$digits)
    ub.rounded <- roundWithZeros(ub, params$digits)
    y.overall.rounded <- roundWithZeros(y.overall, params$digits)
    lb.overall.rounded <- roundWithZeros(lb.overall, params$digits)
    ub.overall.rounded <- roundWithZeros(ub.overall, params$digits)
    if (params$fp_show_col2=='TRUE') {
        additional.cols <- list(es = c(paste(params$fp_col2_str, sep = ""),
                                 paste(y.rounded, " (", lb.rounded, " , ", ub.rounded, ")", sep = ""),
                                 paste(y.overall.rounded, " (", lb.overall.rounded, " , ", ub.overall.rounded, ")", sep = "")))
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
    metric %in% c("RR", "OR", "PETO")    
}

create.plot.data.binary <- function(binary.data, params, res, selected.cov = NULL, include.overall=TRUE){

    plot.data <- create.plot.data.generic  (binary.data, params, res, selected.cov=selected.cov)
        
    # if we have raw data, add it to the additional columns field
    if ((length(binary.data@g1O1) > 0) && (params$fp_show_col3=="TRUE")) {
        plot.data$additional.col.data$cases = c(paste(params$fp_col3_str, sep = ""), 
                                    paste(binary.data@g1O1, " / ", binary.data@g1O1 + binary.data@g1O2, sep = ""), 
                                    paste(sum(binary.data@g1O1), " / ", sum(binary.data@g1O1 + binary.data@g1O2), sep = ""))
    }
    
    if ((length(binary.data@g2O1) > 0) && (params$fp_show_col4=="TRUE")){
        plot.data$additional.col.data$controls = c(paste(params$fp_col4_str, sep = ""),
                                        paste(binary.data@g2O1, " / ", binary.data@g1O1 + binary.data@g2O2, sep = ""),
                                        paste(sum(binary.data@g2O1), " / ", sum(binary.data@g1O1 + binary.data@g2O2), sep = ""))
    }
    
    plot.data
}

create.plot.data.continuous <- function(cont.data, params, res, selected.cov = NULL, include.overall=TRUE){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    plot.data <- create.plot.data.generic  (cont.data, params, res, selected.cov=selected.cov)
    
    plot.data
}

create.plot.data.overall <- function(params, res, studyNames, addRow1Space, selected.cov=NULL){
    scale.str <- "cont"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    }
    # Add space to row 1 for cumulative ma to align study names.
    if (addRow1Space == TRUE) {
        studyNames[1] <- paste("   ", studyNames[1], sep="")
    }
    plot.data <- list( label = c("Studies", studyNames),
                types = c(3, rep(0, length(studyNames))),
                scale = scale.str)
    y <- res[,1]
    lb <- res[,2]
    ub <- res[,3]
    # round results for display.
    y.rounded <- roundWithZeros(y, params$digits)
    lb.rounded <- roundWithZeros(lb, params$digits)
    ub.rounded <- roundWithZeros(ub, params$digits)
    
    if (params$fp_show_col2=='TRUE') {
        additional.cols <- list(es = c(paste(params$fp_col2_str, sep = ""),
                                 paste(y.rounded, " (", lb.rounded, " , ", ub.rounded, ")", sep = "")))
        plot.data$additional.col.data <- additional.cols 
    }      
    #additional.cols <- list(es = c("ES (LL, UL)", 
    #                        paste(y.rounded, " (", lb.rounded, " , ", ub.rounded, ")", 
    #                        sep = "")))
                               
    #plot.data$additional.col.data <- additional.cols               
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
    # this is just scalling the boxes according to the SE
    precision <- NULL
    effect.col.range <- NULL
    effect.col<-forest.data$effects
    # i have kept the "ifs" bellow: when we decide to include more metrics
    # these will be expanded
    if (forest.data$scale == "log")
    {
           precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    }
    else if (forest.data$scale == "cont")
    {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
    } 
   # some heuristics to determine xscale 
    if (min(effect.col$LL)>0 && max(effect.col$UL)>0) {
            effect.col.range <- c(max(0.5*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
                                      }   
    else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)<=0) { 
            effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(-1.5*max(effect.col$ES) , max(effect.col$UL)))
                                      }   
    else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)<=0 && max(effect.col$ES)>0) { 
            effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(2*max(effect.col$ES) , max(effect.col$UL)))
                                      }   
    else if (min(effect.col$LL)<=0 && max(effect.col$UL)>=0 && min(effect.col$ES)>0 && max(effect.col$ES)>0) { 
            effect.col.range <- c(max(-2*min(effect.col$ES) , min(effect.col$LL)), min(1.5*max(effect.col$ES) , max(effect.col$UL)))
                                      }   
    else if (min(effect.col$LL)<=0 && max(effect.col$UL)<0) { 
            effect.col.range <- c(max(2*min(effect.col$ES) , min(effect.col$LL)), min(0.10*max(effect.col$ES) , max(effect.col$UL)))
                                      }
    
   # this is an ugly solution to an uncommon problem                                 
    merge.data <- data.frame(x = forest.data$types[-1], y = effect.col$LL, z = effect.col$UL)
    merge.data <- subset(merge.data, x>0)                                  
    
    if (min(effect.col.range) >= min(merge.data$y)) 
        effect.col.range[1] <- min(merge.data$y)
    if (max(effect.col.range) <= max(merge.data$z)) 
        effect.col.range[2] <- max(merge.data$z)
        
            
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
  if (forest.data$scale == "log" && min(col$range)<0 && max(col$range)>0 ) 
      grid.lines(x=unit(0, "native"), y=0:1)
  else if (forest.data$scale == "cont" && min(col$range)<0 && max(col$range)>0 ) 
      grid.lines(x=unit(0, "native"), y=0:1)
      
  # Assume that last value in col is "All" 
  grid.lines(x=unit(col$ES[length(col$ES)], "native"),
             y=0:1, gp=gpar(lty = summary.line.pat, col= summary.line.col))
  
  if  (forest.data$scale == "cont") {
        if (length(user.ticks) == 0) {
              grid.xaxis( gp=gpar(cex=0.6))
        } else {
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
    #popViewport()
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


    # make the data data.frame and exclude the first element of 
    # types (it's just a clumn heading)
    data.reg <- data.frame(plot.data$effects, types = plot.data$types[-1])
    # data for plot (only keep the studies - not the summaries)
    data.reg <- subset(data.reg, types==0)
    
    # area of circles
    precision = NULL
    mult = 1.96 # again, 1.96 is a convention for scalling, no need to parameterize
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

#####################################
#   meta-regression usage example   #
#####################################

reg.data <- list(label = c("Studies",  "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study4" , "subgroup2" , "study1" , "study2",
               "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "Overall"),
            types = c(3,0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,1,2),
            scale = "log" )

reg.data$additional.col.data <- list( col1= c("xxxxx", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" ,  "subgroup2" , "study1" , "study2",
               "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2", "all results")     )

# these are the effect size, again, identical


#reg.data$effects <- list(ES=log(c(0.1, 1.17, 800, 1.86, 1.05,
#                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 
#                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01,
#                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 2.20, 1.4, 1.5)),
#              LL=log(c( 0.001, 1.03, 1.42, 1, 0.95,
#                       0.87, 1.03, 1.03, 1.42, 0.76, 1.09, 
#                       0.87, 1.03, 1.03, 1.42, 0.76, 1.09,
#                       0.87, 1.03, 1.03, 1.42, 0.99, 1.09, 1.07, 0.8, 1.2)),
#              UL= log(c( 300, 1.32, 10000, 2.51, 2,
#                       1.85, 1.32, 1.32, 3.21, 3.51, 3.71, 
#                       1.85, 1.32, 1.32, 3.21, 3.51, 3.71,
#                       1.85, 1.32, 1.32, 3.21, 3.51, 3.71, 4.35, 4, 1.7)) )
 
#reg.data$effects <- list(
#              ES= c(-10, 10, 40, rep(29, 20), 44, 50,  50) ,
#              LL= c(-20, 1, 20, rep(22, 20), 34, 23, 29)  ,
#              UL= c(10, 15, 10000, rep(34, 20), 59, 72,  70) 
#              ) 
 
                       
#reg.data$effects <- list(
#              ES= log(c(1.8, 0.93, 0.8,  rep(1.1, 20), 1.3,   1.3,  1.3)) ,
#              LL= log(c(1.1, 0.6, 0.78, rep(1.0, 20), 0.9, 0.99, 0.8))  ,
#              UL= log(c(1.9, 1.1,  1.5,  rep(1.2, 20), 1.8,   1.6,  1.6)) 
#              )

#reg.data$effects <- list(
#              ES= log(c(0.5, 1, 40000, rep(29, 20), 44, 50,  50)/17) ,
#              LL= log(c(0.1, 0.95, 20, rep(22, 20), 34, 23, 29)/17)  ,
#              UL= log(c(2, 1.1, 10000000, rep(34, 20), 59, 72,  70)/17) 
#              )

reg.data$effects <- list(
              ES= log(c(1, 0.5, 0.8,  rep(0.3, 20), 1.3,   1.3,  1.1)) ,
              LL= log(c(0.01, 0.3, 0.78, rep(0.2, 20), 0.9, 0.99, 0.8))  ,
              UL= log(c(7, 0.7,  0.9,  rep(0.5, 20), 1.8,   1.6,  1.4)) 
              )
              
              
reg.data$effects <- list(
              ES= log(c(rep(1.1, 25), 1 )) ,
              LL= log(c(rep(0.8, 25), 0.9)),
              UL= log(c(rep(1.4, 25), 1.1)) 
              )
              

# these are the additional stuff that the metaregression will need
# note that covariate values should be as many as the primary studies !!!
# it is not meaningful to plot the summary estimates - overall or subgroup
reg.data$covariate <- list(varname = "lala",
                           values = c( -2, 0, 1.17, 2.97, 1.86, 1.05,
                                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 
                                      1.27, 1.17, 1.17, 1.8, 1.86, 2.01))


reg.data$fitted.line <-list(intercept = -1, slope = 1.7)

### actual use:

meta.regression.plot(reg.data,
                        outpath = "meta_reg_test.png",
                        symSize=2,
                        metric = "anything I want, defeault = effect size",
                        xlabel = "again, whatever i like, default is the covariate name",
                        lweight = 3,
                        lpatern = "dotted",
                        # i prefer plotregion = "n", so i made this the default!
                        #  plotregion = "o"    #will give a more classic feel 
                        mcolor = "darkgreen",
                        regline = TRUE)

forest.plot(reg.data, "lalalalala.png")
