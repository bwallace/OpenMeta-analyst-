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

library("grid")

      
#####################################################
#   functions for data manipulation and forest plot #
#####################################################

###
# TODO 10/13/10
#   1) Move create.plot.data from binary_methods to this module
#   2) Implement create.plot.data.continuous -- refactor out
#        common code to create.plot.data.generic
create.plot.data.generic <- function(om.data, params){
        
}


create.plot.data.continuous <- function(cont.data, params, res, selected.cov = NULL, include.overall=TRUE){
    # Creates a data structure that can be passed to forest.plot
    # res is the output of a call to the Metafor function rma
    plot.data <- list(label = c("Studies", cont.data@studyNames, "Overall"),
                     types = c(3, rep(0, length(binaryData@studyNames)), 2),
                     scale = "log" )

    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    lb <- binaryData@y - mult*binaryData@SE
    ub <- binaryData@y + mult*binaryData@SE


    ### TODO only do this for appropriate metrics
    # i.e., ratios, which will be on the log-scale
    # exponentiate effect sizes and bounds.
    y <- exp(binaryData@y)
    lb <- exp(lb)
    ub <- exp(ub)

    yOverall <- exp(res$b[1])
    lbOverall <- exp(res$ci.lb[1])
    ubOverall <- exp(res$ci.ub[1])

    # round results for display.
    yRounded <- round(y, digits = params$digits)
    lbRounded <- round(lb, digits = params$digits)
    ubRounded <- round(ub, digits = params$digits)
    yOverallRounded <- round(yOverall, digits = params$digits)
    lbOverallRounded <- round(lbOverall, digits = params$digits)
    ubOverallRounded <- round(ubOverall, digits = params$digits)

    additional.cols <- list(es = c("ES (LL, UL)", paste(yRounded, " (", lbRounded, " , ", ubRounded, ")", sep = ""),
                                 paste(yOverallRounded, " (", lbOverallRounded, " , ", ubOverallRounded, ")", sep = "")))
        
    # if we have raw data, add it to the additional columns
    if (length(binaryData@g1O1) > 0) {
        additional.cols$cases = c("Ev/Trt", 
                                    paste(binaryData@g1O1, " / ", binaryData@g1O1 + binaryData@g1O2, sep = ""), 
                                    paste(sum(binaryData@g1O1), " / ", sum(binaryData@g1O1 + binaryData@g1O2), sep = ""))
        additional.cols$controls = c("Ev/Ctrl", 
                                        paste(binaryData@g1O1, " / ", binaryData@g1O1 + binaryData@g1O2, sep = ""),
                                        paste(sum(binaryData@g1O1), " / ", sum(binaryData@g1O1 + binaryData@g1O2), sep = ""))
    }

    plotData$additional.col.data <- additional.cols
    if (!is.null(selected.cov)){
        cov.val.str <- paste("binaryData@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plotData$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }
    
    effects <- list(ES = c(y, yOverall),
                    LL = c(lb, lbOverall),
                    UL = c(ub, ubOverall))
    plotData$effects <- effects
    plotData
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
    additional.data <- length(forest.data$additional.col.data)
  
    additionalColumns <- vector("list", length(forest.data$additional.col.data))
    
    for (j in 1:length(forest.data$additional.col.data)){
        content<-rep(NA, length(forest.data$additional.col.data$cases))
        
        for (i in 1:length(forest.data$additional.col.data$cases)){
          if (forest.data$types[i] != 0)
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], x=1, just = "right", gp = gpar(fontface = font)))
          else
            content[i] <- list(textGrob(forest.data$additional.col.data[[j]][[i]], x=1, just = "right", gp = gpar(fontface = "plain")))
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
        additionalColumns[[j]] <-list(content = content, rows = rows)
    }
    additionalColumns
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


plot.options <- function(forest.data, box.sca = 1) {
    # weights for the boxes
    ## TODO parameterize 1.96
    precision <- NULL
    effect.col.range <- NULL
    effect.col<-forest.data$effects
    if (forest.data$scale == "log") 
    {
          precision <- sqrt(1 / ((log(effect.col$UL) - log(effect.col$LL))/(2*1.96)))
          effect.col.range <- c(min(0.5 , min(effect.col$LL)), min(4 , max(effect.col$UL)))
    }
    else if (forest.data$scale == "cont") 
    {
          precision <- sqrt(1 / ((effect.col$UL - effect.col$LL)/(2*1.96)))
          effect.col.range <- c(max(-3 , min(effect.col$LL)), min(3 , max(effect.col$UL)))
    }   
    effect.col.sizes <- box.sca * precision/max(precision)
    effect.col.width <- unit(3, "inches")
    
    forest.params = list(
        col.gap = unit(3, "mm"),
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
  
  if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) > 0){
    grid.arrows(x=unit(c(LL, 1), c("native", "npc")),
                length=unit(0.05, "inches"))                 
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) < 1  &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0){
    grid.arrows(x=unit(c(UL, 0), c("native", "npc")),
               length=unit(0.05, "inches"))
  }
  else if (convertX(unit(UL, "native"), "npc", valueOnly=TRUE) > 1   &&  convertX(unit(LL, "native"), "npc", valueOnly=TRUE) < 0 ){
    grid.arrows(x=unit(c(ES, 0), c("native", "npc")),
               length=unit(0.05, "inches")) 
    grid.arrows(x=unit(c(ES, 1), c("native", "npc")),
               length=unit(0.05, "inches"))              
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
draw.data.col <- function(col, j, color.overall = "black",
                                color.subgroup = "black",
                                summary.line.col = "darkred",
                                summary.line.pat = "dashed",
                                metric = "Effect size",
                                diam.size=1) {
  pushViewport(viewport(layout.pos.col=j, xscale=col$range))
  
  if (forest.data$scale == "log") 
      grid.lines(x=unit(1, "native"), y=0:1)
  else if (forest.data$scale == "cont") 
      grid.lines(x=unit(0, "native"), y=0:1)
      
  # Assume that last value in col is "All" 
  grid.lines(x=unit(col$ES[length(col$ES)], "native"),
             y=0:1, gp=gpar(lty = summary.line.pat, col= summary.line.col))
  grid.xaxis(gp=gpar(cex=0.6))
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
    additional.cols <- additional.columns(forest.data, "bold")
    effects.col <- effectsize.column(forest.data)
    forest.plot.params <- plot.options(forest.data, box.sca=0.8)
                
    # these are calls to plotting functions
    extra.space <- sum(forest.data$types != 0) 
    height <- length(forest.data$types)+ extra.space
    data.width <- unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)), 
                             forest.plot.params$col.gap)
     
    pushViewport(viewport(layout=grid.layout(height ,2*length(additional.cols)+3,
                            widths=
                               unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                                      forest.plot.params$col.gap,  rep(data.width, length(additional.cols)), forest.plot.params$effect.col.width),
                                      heights=unit(rep(1, height)  , "lines"))))
    
                                      
    ## consider including these as they have no options and i want them to 
    number.cols <- 2 + length(additional.columns)
    
    draw.label.col(study.col, 1)
    
    for (i in 1:length(additional.cols)){
        draw.label.col(additional.cols[[i]], 1+2*i)
    }
    
    ### this could (ahem, should) be better refactored
    # we pull out some values for the effects column that
    # are computed in the plot.params method -- but they're
    # not really 'plot parameters'. they should be computed
    # elsewhere.
    effects.col$range <- forest.plot.params$effect.col.range
    effects.col$sizes <- forest.plot.params$effect.col.sizes
    effects.col$width <- forest.plot.params$effect.col.width
    draw.data.col(effects.col, 2*length(additional.cols)+3,  
                             color.overall = "lightblue",
                             color.subgroup = "yellow",
                             summary.line.col= "red",
                             summary.line.pat = "dashed",
                             metric = "Effect size",
                             diam.size = 1.2)   
    
                     
    popViewport()
    graphics.off()
    
    
    # TODO need to do something about the scaling.
    png(file=outpath, width =25, height = 15, units = "in", res = 144)
    pushViewport(viewport(layout=grid.layout(height ,2*length(additional.cols)+3,
                            widths=
                            unit.c(max(unit(rep(1, length(forest.data$label)), "grobwidth", study.col$content)),
                                   forest.plot.params$col.gap,  
                                   rep(data.width, length(additional.cols)), forest.plot.params$effect.col.width),
                                   heights=unit(rep(1, height)  , "lines"))))
    
     #### consider including these as they have no options and i want them to 
    
    number.cols <- 2 + length(additional.columns)
    
    draw.label.col(study.col, 1)
    
    for (i in 1:length(additional.cols)) {
        draw.label.col(additional.cols[[i]], 1+2*i)
    }
    
    draw.data.col(effects.col, 2*length(additional.cols)+3,  color.overall = "lightblue",
                          color.subgroup = "yellow",
                          summary.line.col= "red",
                          summary.line.pat = "dashed",
                          metric = "Effect size",
                          diam.size = 1.2)   
    
    popViewport()
    graphics.off()
}


### sample usage.
# these are the main data (study names and subgroups)
forest.data <- list( label = c("Studies", "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , 
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "study1" , "study2", 
               "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , 
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "Overall"),
            types = c(3,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,2),
            scale = "cont" )
                    
# these are the rest of the data (optional info that we want to appear in tabular format)
additional.cols <- list(  cases = c("cases", "123456" , "110" , "28", "238" , "150", "155", "305" ,
                            "100" , "110" , "28", "238" , "150", "155", "305" , 
                             "100" , "110" , "28", "238" , "150", "155", "305" ,
                            "100" , "110" , "28", "238" , "150", "155", "305" , "634") ,
              controls =  c("controls", "231" , "123" , "123" ,  "13" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "210" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "123" , "150", "155", "305" , 
                            "100" , "123" , "24" ,  "123" , "123", "123", "123" , "6334537") ,
                rejected1 =  c("rejected1", "99" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "123" , "150", "155", "305" , 
                            "100" , "123" , "24" ,  "123" , "123", "123", "123" , "6334537"),
                rejected2 =  c("rejected2", "99" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "123" , "150", "155", "305" , 
                            "100" , "123" , "24" ,  "123" , "123", "123", "123" , "6334537"),
                rejected3 =  c("rejected3", "1" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "1" , "2", "3", "305" , 
                            "100" , "3" , "4" ,  "123" , "150", "155", "6" , 
                            "100" , "123" , "24" ,  "4" , "5", "7", "123" , "6334537"),
                rejected4 =  c("rejected4", "1" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "1" , "2", "3", "305" , 
                            "100" , "3" , "4" ,  "123" , "150", "155", "6" , 
                            "100" , "123" , "24" ,  "4" , "5", "7", "123" , "6334537"),
                rejected5 =  c("rejected5", "1" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "1" , "2", "3", "305" , 
                            "100" , "3" , "4" ,  "123" , "150", "155", "6" , 
                            "100" , "123" , "24" ,  "4" , "5", "7", "123" , "6334537"),
                rejected6 =  c("rejected6", "1" , "123" , "99" ,  "99" , "150", "155", "305" , 
                            "100" , "110" , "24" ,  "1" , "2", "3", "305" , 
                            "100" , "3" , "4" ,  "123" , "150", "155", "6" , 
                            "100" , "123" , "24" ,  "4" , "5", "7", "123" , "6334537"))

forest.data$additional.col.data <- additional.cols

# these are the effect sizes
effects <- list(ES=c(-1, 1.27, 1.17, 1.17, 2.97, 1.86, 1.05,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20, 1.4, 1.5),
              LL=c(-4, -6.0, 1.03, 1.03, 1.42, 0.46, 0.85,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09, 1.07,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09, 1.07, 0.8, 1.2),
              UL=c(3.28, 1.85, 1.32, 1.32, 6.21, 7.51, 2,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71, 1.35,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71, 1.35, 2.2, 1.7))
                               
                               
forest.data$effects <- effects
forest.plot(forest.data, "test.png")



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
    mult = 1.96 # TODO parameterize
    if (plot.data$scale == "log"){
        precision <- 1 / ((log(data.reg$UL) - log(data.reg$LL))/(2*mult))
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
        symbols(y = log(data.reg$ES), x = cov.values, circles = symSize*radii , inches = FALSE,
              xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
    }
    #certainly there is a better way  ?
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
reg.data <- list(label = c("Studies", "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "study1" , "study2",
               "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "Overall"),
            types = c(3,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,2),
            scale = "cont" )

# these are the effect size, again, identical
reg.data$effects <- list(ES=c(-1, 1.27, 1.17, 1.17, 2.97, 1.86, 1.05,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20, 1.4, 0.1),
              LL=c(-4, -6.0, 1.03, 1.03, 1.42, 0.46, 0.85,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09, -3,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09,
                       0.87, 1.03, 1.03, 1.42, 0.46, 1.09, 1.07, 0.8, 0),
              UL=c(3.28, 1.85, 1.32, 1.32, 6.21, 7.51, 2,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71, 1.35,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71,
                       1.85, 1.32, 1.32, 6.21, 7.51, 3.71, 1.35, 4, 0.2))


# these are the additional stuff that the metaregression will need
# note that covariate values should be as many as the primary studies !!!
# it is not meaningful to plot the summary estimates - overall or subgroup
reg.data$covariate <- list(varname = "lala",
                           values = c(0, 1.27, 0, 1.17, 2.97, 1.86, 1.05,
                                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20,
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
