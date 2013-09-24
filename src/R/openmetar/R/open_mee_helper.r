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
	
	#print("betas:\n")
	#print(matrix(rma.results$b, ncol=1))
	#print("cov:\n")
	#print(rma.results$vb)
	#print(matrix(diag(rma.results$vb), ncol=1))
	#print("a matrix:\n")
	#print(a)
	#print("\n")
	
	new_betas  <- t(a) %*% matrix(rma.results$b, ncol=1)
	new_cov   <- t(a) %*% rma.results$vb %*% a
	new_vars <- diag(new_cov)
	new_lowers <- new_betas - mult*sqrt(new_vars)
	new_uppers <- new_betas + mult*sqrt(new_vars)
	new_se     <- sqrt(new_vars)
	
	res <- data.frame(b=new_betas, ci.lb=new_lowers, ci.ub=new_uppers, se=new_se)
	res
}

adjusted_means_display <- function(rma.results, params, display.data, conf.level=95.0) {

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


cond_means_display <- function(rma.results, params, display.data, reg.data, conf.level, cat.ref.var.and.levels, cond.means.data) {
	# cat.ref.var.and.levels is a list:
	#      keys: names of covariates
	#      values: vector of ref.value and rest of levels in proper order
	#
	# cond.means.data is a list obtained from python:
	#      keys: chosen.cov.name : name of covariate chosen to stratify over
	#            other covariate names: values chosen for these covariates
	
	
	chosen.cov.name = as.character(cond.means.data$chosen.cov.name)
	
	
	##### Generate 'a' matrix
	
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
			chosen.cov.levels <- cat.ref.var.and.levels[[cat.name]]
			chosen.level <- chosen.cov.levels[i]
			block <- make.submatrix.for.strat.var(chosen.cov.levels, ref.var)
			a <- cbind(a,block)
			
		} # end of else
		
	} # end for
	
	cat("A matrix", a)
	# End of a matrix generation
	
	
	res <- linear.combination(a, rma.results, conf.level)
	
	
	#levels <- display.data$levels.display.col
	#levels <- levels[levels!='']
	levels <- chosen.cov.levels
	
	if (chosen_pos-1<1) {
		studies.start.index <- 1
	} else {
		studies.start.index <- sum(display.data$factor.n.levels[1:(chosen_pos-1)])+1
	}
	studies <- display.data$studies.display.col
	studies <- studies[studies!='']
	studies <- studies[studies.start.index:(studies.start.index+length(levels)-1)]
	
	
	res <- c(res, list(levels=levels, studies=studies, chosen.cov.name=chosen.cov.name, cond.means.data=cond.means.data)  )
	return(create.adjusted.regression.display2(res, params))
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


create.adjusted.regression.display2 <- function(res, params) {
	col.labels <- c("Level", "Studies", "Conditional Means", "Lower bound", "Upper bound", "Std. error")
	n.rows = length(res$levels)+1
	
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
	reg.array[2:n.rows, "Conditional Means"] <- coeffs
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

