#forest.path <- paste(params$fp_outpath,sep="")
#plot.data <- create.plot.data.continuous(cont.data, params, res)
#changed.params <- plot.data$changed.params
## list of changed params values
#params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
#changed.params <- c(changed.params, params.changed.in.forest.plot)
#params[names(changed.params)] <- changed.params
## dump the forest plot params to disk; return path to
## this .Rdata for later use
#forest.plot.params.path <- save.data(cont.data, res, params, plot.data)

create.phylogenetic.ma.plot.data <- function (data, res, params, conf.level, metric) {
# creates plot.data for forest.plot to make a forest plot with
# calculates plot sizes and layout
# outputs: forest.data is a list contains the following fields:
#
#   NOTE: last element of effect.disp, effects, label, types are for the 'overall' values found in res
#
# - effects.disp - list with 3 fields:
#     - y.disp - vector of effect sizes in display scale
#     - lb.disp - conf. int. lower bound in display scale
#     - ub.disp - conf. int. upper bound in display scale
#
# - effects - list with 3 fields:
#     - ES - vector of effect sizes in calc. scale
#     - LL - conf. int. lower bound in calc. scale
#     - UL - conf. int. upper bound in calc. scale
#
# - types - vector specifying row types:
#     - 0 - study-level data
#     - 1 - subgroup summary data
#     - 2 - overall summary data
#     - 3 - row of column labels
#     - 4 - blank row (e.g. for empty summary row in right-hand side of cumulative plot)
#     - 5 - overall summary data with unscaled diamond (e.g. for leave-one-out plots)
# 
# - label - vector of row labels of length 1 more than length of effect sizes.
#           First entry is usually "Studies" assuming first row has type 3.
#
# - scale - transformation scale - takes one of the following values:
#     - "standard" - untransformed
#     - "log"
#     - "logit"
#     - "arcsine" 
#
# - options - plot options
#
# - plot range - range of x-values in which to draw plot
	
	forest.data <- list()
	
	#alpha <- 1.0-(conf.level/100.0)
	#mult <- abs(qnorm(alpha/2.0))
	
	# Get yi and vi for overall result
	tmp <- data.frame(yi=res$b, lb=res$ci.lb, ub=res$ci.ub)
	# calling raw.to.trans with metric="GEN" just gives the yi and vi for a given yi and conf.interval
	overall.yi.and.vi <- raw.to.trans(metric="GEN", source.data=tmp, conf.level=conf.level) 
	
	### Make effects.disp sublist. Note: 'raw' is 'display' scale
	effects.disp <- list()
	raw.scale <- trans.to.raw(metric, data, conf.level)
	overall.raw.scale <- trans.to.raw(metric, overall.yi.and.vi, conf.level)
	effects.disp$y.disp  <- c(raw.scale$yi, overall.raw.scale$yi)
	effects.disp$lb.disp <- c(raw.scale$lb, overall.raw.scale$lb)
	effects.disp$ub.disp <- c(raw.scale$ub, overall.raw.scale$ub)
	# attach to forest.data
	forest.data$effects.disp <- effects.disp 
	
	### Make effects sublist
	effects <- list()
	# calling trans.to.raw with metric="GEN" just gets the yi, lb, and ub for a given effect size and variance
	calc.scale <- trans.to.raw(metric="GEN",data, conf.level)
	overall.calc.scale <- trans.to.raw(metric="GEN", overall.yi.and.vi, conf.level)
	effects$ES <- c(calc.scale$yi, overall.calc.scale$yi)
	effects$LL <- c(calc.scale$lb, overall.calc.scale$lb)
	effects$UL <- c(calc.scale$ub, overall.calc.scale$ub)
	# attach to forest.data
	forest.data$effects <- effects
	
	### scale
	scale <- switch(metric,
			SMD = "standard", # Hedges d
			ROM = "log",     # Ln Response Ratio
			OR  = "log",     # Log Odds Ratio
			RD  = "standard", # Rate Difference
			RR  = "log",     # Log Relative Rate
			GEN  = "standard", # generic effect 
			"standard") ##ZCOR = rtoz(),     # Fisher's Z-transform
	# attach to forest.data
	forest.data$scale <- scale
	
	#### label
	"Overall (I^2=94% , P< 1e-06)"
	cutoff = 0.0001
	if (res$pval > cutoff) {
		pval.str <- paste("=", formatC(res$pval, digits = 4, format = "f")) 
	} else {
		pval.str <- paste0("< ", cutoff)
	}
	overall.str <- sprintf("Overall (P %s)", pval.str) # I^2 not in rma.mv ??
	label <- c("Studies",data$slab,overall.str)
	forest.data$label <- label # attach to forest.data
	
	### types
	nstudies <- nrow(data) # number of studies
	types <- c(3,rep(0,nstudies),2)
	forest.data$types <- types # attach types to forest.data
	

	
	######### Oy, using yucky code from before to set options #################
	plot.options <- set.plot.options(params)
	if (params$fp_plot_lb == "[default]") {
		plot.options$plot.lb <- params$fp_plot_lb
	} else {
		plot.lb <- eval(parse(text=paste("c(", params$fp_plot_lb, ")", sep="")))
		#plot.options$plot.lb <- eval(call(transform.name, params$measure))$calc.scale(plot.lb, n)
		plot.options$plot.lb <- raw.scale.single.val.to.trans.scale(plot.lb, metric)
	} 
	
	if (params$fp_plot_ub == "[default]")  {
		plot.options$plot.ub <- params$fp_plot_ub
	} else {
		plot.ub <- eval(parse(text=paste("c(", params$fp_plot_ub, ")", sep="")))
		if (scale == "logit") {
			plot.ub <- min(1, plot.ub)
		}  
		#plot.options$plot.ub <- eval(call(transform.name, params$measure))$calc.scale(plot.ub, n)
		plot.options$plot.lb <- raw.scale.single.val.to.trans.scale(plot.ub, metric)
	} 
	########################## end of yucky code ##############################
	
	
	
	### options
	forest.data$options <- plot.plot.options
	
	### plot.range
	plot.range <- calc.plot.range(effects, plot.options)
	# Calculate a reasonable range for the x-values to display in plot.
	

#	if (scale=="log") {
#		# Plot range is in calc scale, so put back in display scale to update params.
#	
#		plot.range.disp.lower <- trans.scale.single.val.to.raw.scale(plot.range[1], metric)
#		plot.range.disp.upper <- trans.scale.single.val.to.raw.scale(plot.range[2], metric)
#	} else {
#		plot.range.disp.lower <- plot.range[1]
#		plot.range.disp.upper <- plot.range[2]
#	}
#	plot.data$plot.range <- plot.range
	
	forest.data
}

