
#######################################
#       meta-regression scatter       #
#######################################


meta.regression.plot <- function( plot.data,
                                  symSize=1,
                                  lcol = "darkred",
                                  metric = "Effect size",
                                  xlabel= covariate$varname,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE,
                                  outpath) {


	#make the data data.frame and 
	#exclude the first element of types (it's just a clumn heading)
	 plot.data.reg<-data.frame(effects, types = data$types[-1])
	#data for plot (only keep the studies - not the summaries)
	 plot.data.reg <-  subset( reg.data.reg, types==0)
	#area of circles
	
	precision = NULL
	if (plot.data$scale == "log"){
	    precision <- 1 / ((log(data.reg$UL) - log(data.reg$LL))/(2*1.96))
    }
	else if (plot.data$scale == "cont"){
	    precision <- 1 / ((data.reg$UL - data.reg$LL)/(2*1.96))
    }
	
	radii <-  precision/sum(precision)
	# TODO need to do something about the scaling.
	png(file=outpath, width =5  , height = 5, units = "in", res = 144)
	#depends on whether these are natural or log
	if (plot.data$scale == "cont"){
	    symbols(y =  data.reg$ES, x =  covariate$values, circles = symSize*radii , inches = FALSE,
	          xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
    }
	else{ 
	    symbols(y = log(data.reg$ES), x =  covariate$values, circles = symSize*radii , inches = FALSE,
	          xlab = xlabel, ylab = metric, bty = plotregion, fg = mcolor)
    }
	#certainly there is a better way  ?
	# note that i am assuming you have
	#the untransformed coefficient from the meta-reg
	# so i am doing no transformation
	if (regline == TRUE)  {
	   x<-c( min(covariate$values)   , max(covariate$values))
	   y<-c (fitted.line$intercept + 
	   			min(covariate$values)*fitted.line$slope, fitted.line$intercept + 
	   			max(covariate$values)*fitted.line$slope)
	   lines(x, y, col = lcol, lwd = lweight, lty = lpatern)
	}
	graphics.off()
}

### sample usage.

# these are the main data (study names and subgroups)
#identical to what you give the forest plot

data <- list( label = c("Studies", "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "study1" , "study2",
               "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" ,
               "study1" , "study2", "study3" , "subgroup1" , "study3" , "study4" , "subgroup2" , "Overall"),
            types = c(3,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,2),
            scale = "cont" )

# these are the effect size, again, identical
effects <- list(ES=c(-1, 1.27, 1.17, 1.17, 2.97, 1.86, 1.05,
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
covariate <- list(varname = "lala",
                  values = c(0, 1.27, 0, 1.17, 2.97, 1.86, 1.05,
                      1.27, 1.17, 1.17, 2.97, 1.86, 2.01, 1.20,
                      1.27, 1.17, 1.17, 1.8, 1.86, 2.01))

fitted.line <-list(intercept = -1, slope = 1.7)

### actual use:

meta.regression.plot(   symSize=2,
                        metric = "anything I want, defeault = effect size",
                        xlabel = "again, whatever i like, default is the covariate name",
                        lweight = 3,
                        lpatern = "dotted",
                        # i prefer plotregion = "n", so i made this the default!
                        #  plotregion = "o"    #will give a more classic feel 
                        mcolor = "darkgreen",
                        regline = TRUE, 
                        outpath = "meta_reg_test.png")
                        
                        
