# Special methods for helping out OpenMEE

trans.to.raw <- function(metric, source.data, conf.level) {
	# transforms data given in source.data dataframe and returns the result
	# in another dataframe
	#
	# Source data consists of an effect size and variance
	# The return value consists of effect size, lower bound, upper_bound
	#
	# metric is given as the trans-metric name (like escalc expects)
	
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	
	yi.source <- source.data$yi
	vi.source <- source.data$vi
	
	no.change <- function() {
		# just takes the yi and vi and gives back the yi, lb, and ub
		
		yi <- yi.source
		addend = sqrt(vi.source)*mult
		lb <- yi.source - addend
		ub <- yi.source + addend
		
		return(data.frame(yi=yi,lb=lb,ub=ub))
	}
	
	unlog <- function() {
		return(exp(no.change()))
	}
	
	ztor <-function() {
		input <- no.change()
		output <- data.frame(yi=transf.ztor(input$yi),
				            lb=transf.ztor(input$lb),
						    ub=transf.ztor(input$ub),)
		return(output)
	}
	
	switch(metric,
			SMD = no.change(), # Hedges d
			ROM = unlog(),     # Ln Response Ratio
			OR  = unlog(),     # Log Odds Ratio
			RD  = no.change(), # Rate Difference
			RR  = unlog(),     # Log Relative Rate
			ZCOR = ztor()      # Fisher's Z-transform
			)
}



raw.to.trans <- function(metric, source.data, conf.level) {
	# transforms data given in source.data dataframe and returns the result
	# in another dataframe
	#
	# Source data consists of effect size, upper bound, lower bound
	# The return value consists of effect size, variance (yi,vi)
	#
	# metric is given as the trans-metric name (like escalc expects)
	
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	
	yi.source <- source.data$yi
	lb.source <- source.data$lb
	ub.source <- source.data$ub
	
	get.yi.vi <- function(yi,lb,ub) {
		# just takes the yi,lb,ub and gives the yi, and vi
		vi <- ((ub-yi)/mult)^2
		vi <- ifelse(is.na(vi), ((ub-yi)/mult)^2, vi)
		vi <- ifelse(is.na(vi), ((ub-lb)/(2*mult))^2, vi)
		
		return(data.frame(yi=yi,vi=vi))
	}
	
	no.change <- function() {
		return(get.yi.vi(yi.source, lb.source, ub.source))
	}
	
	relog <- function() {
		yi <- log(yi.source)
		lb <- log(lb.source)
		ub <- log(ub.source)
		
		return(get.yi.vi(yi, lb, ub))
	}
	
	rtoz <-function() {
		yi <- transf.rtoz(yi.source)
		lb <- transf.rtoz(lb.source)
		ub <- transf.rtoz(ub.source)
		
		return(get.yi.vi(yi,lb,ub))
	}
	
	switch(metric,
			SMD = no.change(), # Hedges d
			ROM = relog(),     # Ln Response Ratio
			OR  = relog(),     # Log Odds Ratio
			RD  = no.change(), # Rate Difference
			RR  = relog(),     # Log Relative Rate
			ZCOR = rtoz()      # Fisher's Z-transform
	)
}


# Helpers for regression to show categorical covariate means instead of differences

linear.combination <- function(a, rma.results, conf.level=95.0) {
	# a is a matrix describing how to combine the
	# intercept and a difference to get a mean
	#
	# Ex. 'a' matrix:
	#           A B C D
	# intercept 1 0 0 0
	#        x1 1 1 0 0
	#        x2 1 0 1 0
	# 	     x3 1 0 0 1
	
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	a <- t(a) # need to transpose for math to be sane
	
	new_betas  <- t(a) %*% matrix(rma.results$b, ncol=1)
	new_cov   <- t(a) %*% rma.results$vb %*% a
	new_vars <- diag(new_cov)
	new_lowers <- new_betas - mult*sqrt(new_vars)
	new_uppers <- new_betas + mult*sqrt(new_vars)
	new_se     <- sqrt(new_vars)
	
	res <- data.frame(b=new_betas, ci.lb=new_lowers, ci.ub=new_uppers, se=new_se)
	res
}

adjusted_means_display <- function(rma.results, params, display.data) {

	conf.level <- params$conf.level
	
	# Generate 'a' matrix
	num.levels = display.data$factor.n.levels[1] # just look at the first covariate for now
	a <- rep(1,num.levels)
	for (one.pos in 2:num.levels) {
		col = c(rep(0,one.pos-1),1,rep(0,num.levels-one.pos))
		a <- c(a,col)
	}
	a <- matrix(a, nrow=num.levels)
	
	levels <- display.data$levels.display.col
	levels <- levels[levels!='']
	studies <- display.data$studies.display.col
	studies <- studies[studies!='']
			
	res <- linear.combination(a, rma.results, conf.level)
	res <- c(res, list(levels=levels,
			           studies=studies))
	return(create.adjusted.regression.display(res, params))
}


cond_means_calculation <- function(rma.results, params, reg.data, cat.ref.var.and.levels, cond.means.data) {
	chosen.cov.name = as.character(cond.means.data$chosen.cov.name)
	conf.level <- params$conf.level
	chosen.cov.levels <- cat.ref.var.and.levels[[chosen.cov.name]]
	
	a <- generate.a.matrix(reg.data, cat.ref.var.and.levels, cond.means.data)
	chosen_pos <- get.chosen.cov.position(chosen.cov.name, reg.data)
	
	res <- linear.combination(a, rma.results, conf.level)
	result <- list(res=res, chosen.cov.levels=chosen.cov.levels, chosen_pos=chosen_pos, chosen.cov.name=chosen.cov.name)
}


get.chosen.cov.position <- function(chosen.cov.name, reg.data) {
	count <- 0
	for (i in 1:length(reg.data@covariates)) {
		cov <- reg.data@covariates[[i]]
		cov.name <- cov@cov.name
		cov.type <- cov@cov.type
		
		
		if (cov.type=="factor") {
			count <- count + 1 # position in categorical covariates
			#cat.cov.names <- c(cat.cov.names, cov.name)
			if (cov.name == chosen.cov.name) chosen.pos <- count
		}
	}
	chosen.pos
}

generate.a.matrix <- function(reg.data, cat.ref.var.and.levels, cond.means.data) {
	#####@@@@@@@@@@ Generate 'a' matrix #########################
	chosen.cov.name = as.character(cond.means.data$chosen.cov.name)
	chosen.cov.levels <- cat.ref.var.and.levels[[chosen.cov.name]]
	
	# First make a list of continous and catagorical covariates as they appear in the mods array (and a matrix)
	cont.cov.names <- c()
	cat.cov.names <- c()
	for (i in 1:length(reg.data@covariates)) {
		cov <- reg.data@covariates[[i]]
		cov.name <- cov@cov.name
		cov.type <- cov@cov.type
		
		if (cov.type=="continuous") {
			cont.cov.names <- c(cont.cov.names, cov.name)
		}
		if (cov.type=="factor") {
			cat.cov.names <- c(cat.cov.names, cov.name)
		}
	}
	
	num.rows <- length(cat.ref.var.and.levels[[chosen.cov.name]])
	a <- cbind(c(), rep(1,num.rows))
	
	# add in data for continuous variables
	for (cont.name in cont.cov.names) {
		col <- rep(cond.means.data[[cont.name]],num.rows)
		a <- cbind(a,col)
	}
	
	# add in data for categorical variables
	count = 0
	for (cat.name in cat.cov.names) {
		count = count + 1
		ref.var <- cat.ref.var.and.levels[[cat.name]][[1]]
		n.cols  <- length(cat.ref.var.and.levels[[cat.name]]) - 1 # exclude reference var
		
		if (cat.name != chosen.cov.name) {
			value   <- cond.means.data[[cat.name]]
			levels <- cat.ref.var.and.levels[[cat.name]]
			levels.min.ref <- levels[2:length(levels)]
			
			new.row <- (levels.min.ref==value)*1
			block <- matrix(new.row, byrow=TRUE, nrow=num.rows, ncol=length(levels.min.ref))
			a <- cbind(a, block)
			
		} else { # this is the chosen categorical variable that we stratify over
			chosen_pos = count
			#chosen.cov.levels <- cat.ref.var.and.levels[[cat.name]]
			chosen.level <- chosen.cov.levels[i]
			block <- make.submatrix.for.strat.var(chosen.cov.levels, ref.var)
			a <- cbind(a,block)
			
		} # end of else
		
	} # end for
	
	cat("A matrix", a) 
	a
############## End of a matrix generation ################
}

# Makes a subset of an BinaryData or ContinuousData object with the:
# study names
# study years
# point estimates
# standard errors

get.subset <- function(data, rows, make.unique.names=FALSE) {
	data@study.names <- data@study.names[rows]
	data@y <- data@y[rows]
	data@SE <- data@SE[rows]
	
	if (length(data@covariates) > 0) {
		for (j in 1:length(data@covariates)) {
			# put covariate data into two arrays, for continuous and factor covariates.
			data@covariates[[j]]@cov.vals <- data@covariates[[j]]@cov.vals[rows]
		}
	}
	
	if (make.unique.names) {
		data@study.names <- as.character(1:length(rows))
	}
	
	data
} 


boot.cond.means.display <- function (omdata, coeffs.and.cis, params, cat.ref.var.and.levels, cond.means.data) {
	cov.data <- extract.cov.data(omdata)
	display.data <- cov.data$display.data
	chosen.cov.name = as.character(cond.means.data$chosen.cov.name)

	levels     <- cat.ref.var.and.levels[[chosen.cov.name]]
	chosen_pos <- get.chosen.cov.position(chosen.cov.name, omdata)
	
	# Display proper numbers for studies column
	if (chosen_pos-1<1)
		studies.start.index <- 1
	else
		studies.start.index <- sum(display.data$factor.n.levels[1:(chosen_pos-1)])+1
	studies <- display.data$studies.display.col
	studies <- studies[studies!='']
	studies <- studies[studies.start.index:(studies.start.index+length(levels)-1)]
	
	
	res <- list(b=coeffs.and.cis$b, ci.lb=coeffs.and.cis$ci.lb, ci.ub=coeffs.and.cis$ci.ub,
				levels=levels, studies=studies,
				chosen.cov.name=chosen.cov.name, cond.means.data=cond.means.data)
	return(create.cond.means.regression.display(res, params, bootstrap=TRUE))
}


cond_means_display <- function(rma.results, params, display.data, reg.data, cat.ref.var.and.levels, cond.means.data) {
	# cat.ref.var.and.levels is a list:
	#      keys: names of covariates
	#      values: vector of ref.value and rest of levels in proper order
	#
	# cond.means.data is a list obtained from python:
	#      keys: chosen.cov.name : name of covariate chosen to stratify over
	#            other covariate names: values chosen for these covariates
	
	
	# returns coefficients, uppers, lowers, and se
	cond_means_result <- cond_means_calculation(rma.results, params, reg.data, cat.ref.var.and.levels, cond.means.data)
	res             <- cond_means_result$res
	levels          <- cond_means_result$chosen.cov.levels
	chosen_pos      <- cond_means_result$chosen_pos
	chosen.cov.name <- cond_means_result$chosen.cov.name
	
	#levels <- display.data$levels.display.col
	#levels <- levels[levels!='']
	#levels <- chosen.cov.levels
	
	if (chosen_pos-1<1) {
		studies.start.index <- 1
	} else {
		studies.start.index <- sum(display.data$factor.n.levels[1:(chosen_pos-1)])+1
	}
	studies <- display.data$studies.display.col
	studies <- studies[studies!='']
	studies <- studies[studies.start.index:(studies.start.index+length(levels)-1)]
	
	
	res <- c(res, list(levels=levels, studies=studies, chosen.cov.name=chosen.cov.name, cond.means.data=cond.means.data)  )
	return(create.cond.means.regression.display(res, params))
}



make.submatrix.for.strat.var <- function(levels, ref) {
	levels.min.ref <- levels[2:length(levels)]
	block = c()
	for (lvl in levels) {
		new.row <- (levels.min.ref == lvl)*1
		block <- rbind(block, new.row)
	}
	block
}


create.cond.means.regression.display <- function(res, params, bootstrap=FALSE) {
	if (bootstrap)
		col.labels <- c("Level", "Studies", "Conditional Means", "Lower bound", "Upper bound")
	else
		col.labels <- c("Level", "Studies", "Conditional Means", "Lower bound", "Upper bound", "Std. error")
	n.rows = length(res$levels)+1
	
	reg.array <- array(dim=c(n.rows, length(col.labels)), dimnames=list(NULL, col.labels))
	reg.array[1,] <- col.labels
	digits.str <- paste("%.", params$digits, "f", sep="")
	coeffs <- sprintf(digits.str, res$b)
	if (!bootstrap)
		se <- round.display(res$se, digits=params$digits)
	lbs <- sprintf(digits.str, res$ci.lb)
	ubs <- sprintf(digits.str, res$ci.ub)
	
	# add data to array
	reg.array[2:n.rows, "Level"] <- res$levels
	reg.array[2:n.rows, "Studies"] <- res$studies
	reg.array[2:n.rows, "Conditional Means"] <- coeffs
	if (!bootstrap)
		reg.array[2:n.rows, "Std. error"] <- se
	reg.array[2:n.rows, "Lower bound"] <- lbs
	reg.array[2:n.rows, "Upper bound"] <- ubs
	
	arrays <- list(arr1=reg.array)
	metric.name <- pretty.metric.name(as.character(params$measure)) 
	
	blurb <- paste("\nThese are the conditional means for '",res$chosen.cov.name, "',\nstratified over its levels given the following values for the other covariates:\n", sep="")
	for (name in names(res$cond.means.data)) {
		if (name != 'chosen.cov.name') {
			blurb <- paste(blurb, name, " = ", res$cond.means.data[[name]], "\n", sep="")
		}
	}
	
	if (bootstrap)
		model.title <- paste("Bootstrapped Meta-Regression-Based Conditional Means\n\nMetric: ", metric.name, "\n", blurb, sep="")
	else
		model.title <- paste("Meta-Regression-Based Conditional Means\n\nMetric: ", metric.name, "\n", blurb, sep="")
	reg.disp <- list("model.title" = model.title, "table.titles" = c("Model Results"), "arrays" = arrays, "MAResults" = res)
	
	class(reg.disp) <-  "summary.display"
	return(reg.disp)
}



create.adjusted.regression.display <- function(res, params) {
	col.labels <- c("Level", "Studies", "Adjusted Means", "Lower bound", "Upper bound", "Std. error")
	n.rows = length(res$b)+1
	
	reg.array <- array(dim=c(n.rows, length(col.labels)), dimnames=list(NULL, col.labels))
	reg.array[1,] <- col.labels
	digits.str <- paste("%.", params$digits, "f", sep="")
	coeffs <- sprintf(digits.str, res$b)
	se <- round.display(res$se, digits=params$digits)
	lbs <- sprintf(digits.str, res$ci.lb)
	ubs <- sprintf(digits.str, res$ci.ub)
	
	# add data to array
	reg.array[2:n.rows, "Level"] <- res$levels
	reg.array[2:n.rows, "Studies"] <- res$studies
	reg.array[2:n.rows, "Adjusted Means"] <- coeffs
	reg.array[2:n.rows, "Std. error"] <- se
	reg.array[2:n.rows, "Lower bound"] <- lbs
	reg.array[2:n.rows, "Upper bound"] <- ubs
	
	arrays <- list(arr1=reg.array)
	metric.name <- pretty.metric.name(as.character(params$measure)) 
	model.title <- paste("Meta-Regression Adjusted Means\n\nMetric: ", metric.name, sep="")
	reg.disp <- list("model.title" = model.title, "table.titles" = c("Model Results"), "arrays" = arrays, "MAResults" = res)
	
	class(reg.disp) <-  "summary.display"
	return(reg.disp)
}

###### 
failsafe.wrapper <- function(data, type="Rosenthal", alpha=.05, target=NULL, digits=4) {
	# wraps metafor fsn() to yield results suitable for use in OpenMEE results window output
	
	res <- fsn(yi=data$yi, vi=data$vi, type=type, alpha=alpha, target=target, digits=digits)
	
	#results.info <- c(list(Summary=list(type="vector", description="Failsafe Analysis Summary")),
	#		fsn.info())
	
	summary_str <- paste(capture.output(res), collapse="\n")
	
	
	results <- list("Summary"  = res,
			        "res"      = c(list(Summary=summary_str), res),
					"res.info" = fsn.info()
					)
	results
}

fsn.info <- function() {
	list(
		Summary = list(type="vector", description="Failsafe Analysis Summary"),
		type   = list(type="vector", description='the method used'),
		fsnum  = list(type="vector", description='the calculated fail-safe N.'),
		alpha  = list(type="vector", description='the target alpha level.'),
		pval   = list(type="vector", description='the p-value of the observed results. NA for the Orwin method.'),
		meanes = list(type="vector", description='the average effect size of the observed results. NA for the Rosenthal method.'),
		target = list(type="vector", description= 'the target effect size. NA for the Rosenthal and Rosenberg methods.')
	)
}


############################ Funnel Plot Code #################################
funnel.wrapper <- function(fname, data, params, ...) {
	# fname: the name of the function that would have been called if this were
	#        a regular meta-analysis
	# data: binary or continuous data object
	# params: parameters for function specified by fname
	# ... : parameters for funnel()

	
	# set data (avoid some annoying copy pasta, too many calories)
	binary.data <- data
	cont.data <- data
	
	# run meta-analysis	
	res <- switch(fname,
                  binary.fixed.inv.var=rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
						  level=params$conf.level, digits=params$digits, method="FE", add=c(params$adjust,params$adjust),
						  to=c(as.character(params$to), as.character(params$to))),
		  		  binary.random=rma.uni(yi=binary.data@y, sei=binary.data@SE, 
						  slab=binary.data@study.names,
						  method=params$rm.method, level=params$conf.level,
						  digits=params$digits,
						  add=c(params$adjust,params$adjust),
						  to=as.character(params$to)),
	              continuous.fixed=rma.uni(yi=cont.data@y, sei=cont.data@SE, 
	 		              slab=cont.data@study.names,
	 		              method="FE", level=params$conf.level,
	 		              digits=params$digits),
	              continuous.random = rma.uni(yi=cont.data@y, sei=cont.data@SE, 
	 		              slab=cont.data@study.names,
	 		              method=params$rm.method, level=params$conf.level,
	 		              digits=params$digits)
		  		) # end of switch
	
	funnel.params <- list(...)
	funnel.plot.data.path <- save.funnel.data(res=res, funnel.params=funnel.params)
	
	# draw plot & save funnel data
	plot.path = paste(funnel.plot.data.path, ".png", sep="")
	make.funnel.plot(plot.path, res, funnel.params)
	
	results <- list(
					images=c("Funnel Plot"=plot.path),
					plot_params_paths=c("Funnel Plot"=funnel.plot.data.path),
					References="funnel plot reference placeholder"
					)
					
	

}

make.funnel.plot <- function(plot.path, res, funnel.params) {
	# make actual plot 
	if (length(grep(".png", plot.path)) != 0){
		png(file=plot.path, width=600, height=600)
	}
	else{
		pdf(file=plot.path) # the pdf device seems to not like setting height and width, width=600, height=600)
	}
	
	do.call(funnel, c(list(res), funnel.params))
	#funnel(res, ...)
	
	graphics.off()
}

regenerate.funnel.plot <- function(out.path, plot.path, edited.funnel.params=NULL) {
	# Used when saving or editing the plot
	
	# out.path is the path to r_tmp/{timestamp}* or whatever
	
	# load res and funnel.params in to function workspace
	load(paste(out.path, ".res", sep=""))
	
	# load the stored funnel params when we are just saving, not editing
	if (is.null(edited.funnel.params)) {
		load(paste(out.path, ".funnel.params", sep=""))
	}
	else {
		funnel.params <- edited.funnel.params
		save.funnel.data(res, funnel.params, out.path=out.path)
	}
	
	make.funnel.plot(plot.path, res, funnel.params)
}

get.funnel.params <- function(out.path) {
	# accessor for python to load the stored funnel params
	load(paste(out.path, ".funnel.params", sep=""))
	funnel.params
}

save.funnel.data <- function(res, funnel.params, data=NULL, params=NULL, out.path=NULL) {
	# adapted from save.data() in utilities.r
	
	# save the data, result and plot parameters to a tmp file on disk
	if (is.null(out.path)){
		# by default, we use thecurrent system time as a 'unique enough' filename
		out.path <- paste("r_tmp/", 
				as.character(as.numeric(Sys.time())), sep="")
	}
	
	#save(data, file=paste(out.path, ".data", sep=""))
	save(res, file=paste(out.path, ".res", sep=""))
	#save(params, file=paste(out.path, ".params", sep=""))
	save(funnel.params, file=paste(out.path, ".funnel.params", sep=""))
	out.path
}

###################### end of funnel plot code ################################

###################### Histogram code #########################

exploratory.plotter <- function(data, params, plot.type) {
	# Main function to call from python side to make a histogram or scatterplot
	
	plot.data.path = save.exploratory.data(data, params)
	
	# draw plot & save funnel data
	plot.path = paste(plot.data.path, ".png", sep="")
	
	if (plot.type=="HISTOGRAM") {
		make.histogram(plot.path, data, params)
		images <- c("Histogram"=plot.path)
		plot_params_paths <- c("Histogram"=plot.data.path)
	}
	else if (plot.type=="SCATTERPLOT") {
		make.scatterplot(plot.path, data, params)
		images <- c("Scatterplot"=plot.path)
		plot_params_paths <- c("Scatterplot"=plot.data.path)
	}
	else
		stop("Unrecognized plot type")
	
	
	
	results <- list(
			images=images,
			plot_params_paths=plot_params_paths
			#References="funnel plot reference placeholder"
	)
	
	
	
}

make.histogram <- function(plot.path, data, params) {
	# make actual plot 
	if (length(grep(".png", plot.path)) != 0) {
		png(file=plot.path, width=600, height=600)
	}
	else{
		pdf(file=plot.path) # the pdf device seems to not like setting height and width, width=600, height=600)
	}
	
	qplot_param_keys = c("xlab","ylab","xlim", "ylim", "binwidth")
	geom_histogram_keys = c("binwidth")
	geom_bar_keys = c("binwidth", "fill","color")
	# 'count_key_name': is not the actual name of the parameter,
	#           the parameter appears to be just the first positional argument
	# 'low','high': can be the name of a color or rgb e.g. "#132B43"
	scale_fill_gradient_keys = c("name","low","high")
	
	# parse params
	qplot_params <- list()
	geom_histogram_params <- list()
	geom_bar_params <- list()
	scale_fill_gradient_params <- list()
	for (p in names(params)) {
		if (p %in% qplot_param_keys) {
			qplot_params[[p]] <- params[[p]]
		}
		if (p %in% geom_histogram_keys) {
			geom_histogram_params[[p]] <- params[[p]]
		}
		if (p %in% geom_bar_keys) {
			geom_bar_params[[p]] <- params[[p]]
		}
		if (p %in% scale_fill_gradient_keys) {
			scale_fill_gradient_params[[p]] <- params[[p]]
		}
	}
	
	if (params[['GRADIENT']]) {
		params.for.qplot <- c(list(data), qplot_params)
		myplot <- do.call(qplot, params.for.qplot) + geom_histogram(aes(fill = ..count..)) + do.call(scale_fill_gradient, scale_fill_gradient_params)			                             
	} else { # gradient
		# no gradient
		myplot <- do.call(qplot, c(list(data), qplot_params)) + do.call(geom_bar, geom_bar_params)
	}
	
	print(myplot)
	graphics.off()
}

get.exploratory.params <- function(out.path) {
	# accessor for python to load the stored histogram params
	load(paste(out.path, ".params", sep=""))
	params
}

save.exploratory.data <- function(data, params, out.path=NULL) {
	# save the data, result and plot parameters to a tmp file on disk
	if (is.null(out.path)){
		# by default, we use thecurrent system time as a 'unique enough' filename
		out.path <- paste("r_tmp/", 
				as.character(as.numeric(Sys.time())), sep="")
	}
	
	save(data, file=paste(out.path, ".data", sep=""))
	save(params, file=paste(out.path, ".params", sep=""))
	out.path
}

# FOR SCATTERPLOT OR HISTOGRAM
regenerate.exploratory.plot <- function(out.path, plot.path, plot.type, edited.params=NULL) {
	# Used when saving or editing the plot
	
	# out.path is the path to r_tmp/{timestamp}* or whatever
	
	# load data and params in to function workspace
	load(paste(out.path, ".data", sep=""))
	
	# load the stored params when we are just saving, not editing
	if (is.null(edited.params)) {
		load(paste(out.path, ".params", sep=""))
	}
	else {
		params <- edited.params
		save.exploratory.data(data, params, out.path=out.path)
	}
	
	if (plot.type=="SCATTERPLOT")
		make.scatterplot(plot.path, data, params)
	else if (plot.type=="HISTOGRAM")
		make.histogram(plot.path, data, params)
	else
		stop("unrecognized plot type")
}
############## End of Histogram code #################################

########################### Scatterplot code #################################
make.scatterplot <- function(plot.path, data, params) {
	if (length(grep(".png", plot.path)) != 0){
		png(file=plot.path, width=600, height=600)
	}
	else{
		pdf(file=plot.path) # the pdf device seems to not like setting height and width, width=600, height=600)
	}
	
	myplot <- do.call(qplot, c(list(data$x, data$y, data=data), params))
	#qplot(data$xvar, data$yvar, data=data)
	print(myplot)
	graphics.off()
}
######################## End of scatterplot code #############################

model.building <- function(data, full.mods, reduced.mods, method, level, digits, btt=NULL) {
	# This is s a thin wrapper to metafor's meta regression functionality
	# in order to let R do the dummy coding for us
	#
	# mods.str: string to be passed to metafor to implement the moderators
	#     e.g. ~ gfactor(alloc)+ablat+gfactor(country)+gfactor(alloc):gfactor(country)
	# mods: list(numeric=c(...numeric moderators...),
	#            categorical=c(...categorical moderators...),
	#            interactions=list("A:B"=c("A","B"),"B:C"=c("B",C"),...)
	#            )
	#     Note that the interaction names should be as they appear in the mods
	#     string formula
	# data: should be a dataframe of the type that metafor likes ie
	# yi and vi for the effect and variance columns
	# slab holds study names
	# the parts that are 'factors' have already been made in to factors with
	# the appropriate reference values
	
	full.mods.str <- make.mods.str(full.mods)
	reduced.mods.str <- make.mods.str(reduced.mods)
	
	# obtain regression result rma.uni
	full.res    <- regression.wrapper(data, full.mods.str, method, level, digits,btt)
	reduced.res <- regression.wrapper(data, reduced.mods.str, method, level, digits,btt)
	
	res <- anova(full.res, reduced.res, digits=digits)
	
	# temporarily widen console for printing
	old.width <- getOption("width")
	options(width=1000)
	summary.str <- paste(capture.output(res), collapse="\n") # convert print output to a string
	options(width=old.width)
	
	results <- list(#"images"=images,
			"Summary"=summary.str,
			#"plot_names"=plot.names,
			#"plot_params_paths"=plot.params.paths,
			"res"=res,
			"res.info"=model.building.value.info())
}

model.building.value.info <- function() {
    list(
        fit.stats.f = list(type="vector", description="log-likelihood, deviance, AIC, BIC, and AICc for the full model."),
        fit.stats.r = list(type="vector", description="log-likelihood, deviance, AIC, BIC, and AICc for the reduced model."),
        p.f         = list(type="vector", description="number of parameters in the full model."),
        p.r         = list(type="vector", description="number of parameters in the reduced model."),
        LRT         = list(type="vector", description="likelihood ratio test statistic."),
        pval        = list(type="vector", description="p-value for the likelihood ratio test."),
        QE.f        = list(type="vector", description="test statistic for the test of (residual) heterogeneity from the full model."),
        QE.r        = list(type="vector", description="test statistic for the test of (residual) heterogeneity from the reduced model."),
        tau2.f      = list(type="vector", description="estimated  2 value from the full model."),
        tau2.r      = list(type="vector", description="estimated  2 value from the reduced model."),
        R2          = list(type="vector", description="amount of (residual) heterogeneity in the reduced model that is accounted for in the full model. NA( for fixed-effects models or if the amount of heterogeneity in the reduced model is equal to zero. This can be regarded as a pseudo R2 statistic (Raudenbush, 2009).")
    )
}
