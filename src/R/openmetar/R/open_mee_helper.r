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
	return(create.adjusted.regression.display(res, params, display.data))
	


}



create.adjusted.regression.display <- function(res, params, display.data) {
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

