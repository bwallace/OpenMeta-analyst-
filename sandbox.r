fillin.2x2.simpler <- function(c11=NA, c12=NA, c21=NA, c22=NA, 
		r1sum =NA, r2sum=NA, 
		c1sum=NA, c2sum=NA,
		total=NA)
{
	X<- matrix(c(
		# c11, c12, c21, c22 
		1,   0,   0,   0,  #c11
		0,   1,   0,   0,  #c12
		0,   0,   1,   0,  #c21
		0,   0,   0,   1,  #c22
		1,   1,   0,   0,  #r1sum=c11+c12
		0,   0,   1,   1,  #r2sum=c21+c22
		1,   0,   1,   0,  #c1sum=c11+c21
		0,   1,   0,   1,  #c2sum=c12+c22
		1,   1,   1,   1), #total=c11+c12+c21+c22
		ncol=4, byrow=TRUE)

	y <- c(c11, c12, c21, c22, r1sum, r2sum, c1sum, c2sum, total)
	y_star <- y[!is.na(y)] # entries of y that are not na
	
	# just go away .... lm doesn't work unless length(y_star) is at least two.. dimensions throw up
	if (length(y_star) < 2)
		return(NA)

	X_star <- X[!is.na(y),]
	
	intermediate_result <- lm(y_star~ X_star[,1] + X_star[,2] + X_star[,3] + X_star[,4] +(-1))
	
	coef <- intermediate_result$coefficients
	# decide which values are ok to output
	X_masked <- matrix(rep(is.na(coef),9), nrow=9, byrow=TRUE) * X  # coefficients remain only if they depend on a value from coef that is NA
	uncomputable <- (X_masked) %*% c(1,1,1,1) > 0 # list of rows which are uncomputable
	coef2 <- coef
	coef2[is.na(coef)] <- 0 # make things work arithmetically
	
	y_computed <- X%*%coef2
	y_computed[uncomputable] = NA # get rid of bogus entries
	y_computed <- as.list(y_computed)
	
	names(y_computed) <- c("c11", "c12", "c21", "c22", "r1sum", "r2sum", "c1sum", "c2sum", "total" )
	
	results <- list(coefficients=y_computed, residuals=intermediate_result$residuals, doubleyous=coef)
	#results
	#y_computed
}
