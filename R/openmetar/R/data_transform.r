isnt.null <- function(x){
    # some syntactic sugar.
    ! is.null(x)    
}

isnt.na <- function(x) {
	!is.na(x)
}

IMAGINARY.THRESHOLD <- 1E-8


############################ 
# Binary data calculation  #
############################

gimpute.bin.data <- function(bin.data) {
	# Imputes binary 2x2 tables from fields in the bin.data frame parameter
	# 
	# a,b,c and d are the respective entries for the 2-x-2 table. they denote 
	# treated events, treated total, control events, and control total, respectively
	#
	# There will be two sets of possible results for each parameter since the solution involves a quadratic
	
	metric <- as.character(bin.data[["metric"]])
	est    <- bin.data[["estimate"]]
	lower  <- bin.data[["lower"]]
	upper  <- bin.data[["upper"]]
	N_1    <- bin.data[["N_A"]]
	N_0    <- bin.data[["N_B"]]
	conf.level <- bin.data[["conf.level"]]
	
	# See if we have enough inputs to proceed
	est_low_up_ok <- isnt.null(est) & (isnt.null(lower) | isnt.null(upper))
	est_low_up_ok <- est_low_up_ok | (isnt.null(lower) & isnt.null(upper))
	inputs_sufficient <- isnt.null(metric) & isnt.null(N_1) & isnt.null(N_0) &
						 est_low_up_ok & isnt.null(conf.level)
	if (!inputs_sufficient) {
		print("Not enough inputs to back-calculate binary table!, exiting gimpute.bin.data..")
		return(list(FAIL=NA))
	}
	
	# Convert NULL to NA (we know the other values are not NULL already)
	if (is.null(est))   est   <- NA
	if (is.null(lower)) lower <- NA
	if (is.null(upper)) upper <- NA
	
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	n <- N_0 + N_1
	
	# Calculates the estimate, low, and high if one of the three is NA, assumes
	# symmetric distribution
	calc.d.and.b <- function (d=NA, d_L=NA, d_U=NA) {
		if (is.na(d))   d   <- (d_L + d_U)/2;
		if (is.na(d_U)) d_U <- 2*d - d_L;
		if (is.na(d_L)) d_L <- 2*d - d_U;
		
		b <- ((d_U - d) / mult)^2
		res <- list(d=d, d_U=d_U, d_L=d_L, b=b)
	}

	impute.from.RD <- function () {
		res <- calc.d.and.b(d=est, d_L=lower, d_U=upper)
		d <- res[["d"]]; b <- res[["b"]]
		
		A <- n;
		B <- (2*N_0*d-n);
		C <- N_0*(N_1*b-d*(1-d));
		
		# calculate proportions
		p0.op1 <- (-B+sqrt(B^2-4*A*C))/(2*A)
		p0.op2 <- (-B-sqrt(B^2-4*A*C))/(2*A)
		p1.op1 <- d + p0.op1
		p1.op2 <- d + p0.op2

		res <- list(op1=list(p0=p0.op1, p1=p1.op1), op2=list(p0=p0.op2, p1=p1.op2))
	}
	
	impute.from.LOR <- function () {
		res <- calc.d.and.b(d=log(est), d_L=log(lower), d_U=log(upper))
		d <- res[["d"]]; b <- res[["b"]]
		
		d <- exp(d) # convert OR back to normal scale (not log)
		
		A <- N_0*(1-d)^2+b*d*N_0*N_1
		B <- -1*(2*N_0*(1-d)+b*d*N_0*N_1)
		C <- N_0 + d*N_1
		
		# calculate proportions
		p0.op1 <- (-B+sqrt(B^2-4*A*C))/(2*A) 
		p0.op2 <- (-B-sqrt(B^2-4*A*C))/(2*A) 
		p1.op1 <- d*p0.op1/(d*p0.op1+1-p0.op1)
		p1.op2 <- d*p0.op2/(d*p0.op2+1-p0.op2)
		
		res <- list(op1=list(p0=p0.op1, p1=p1.op1), op2=list(p0=p0.op2, p1=p1.op2))
		return(res)
	}
	
	impute.from.LRR <- function () {
		res <- calc.d.and.b(d=log(est), d_L=log(lower), d_U=log(upper))
		d <- res[["d"]]; b <- res[["b"]]
		
		d <- exp(d)
		
		# calculate proportions
		p0.op1 <- (N_0+d*N_1)/(d*(b*N_1*N_0+N_1+N_0))
		p1.op1 <- p0.op1*d
		
		res <- list(op1=list(p0=p0.op1, p1=p1.op1))
	}
	
	res <- switch(metric, "RD"=impute.from.RD(),
			              "OR"=impute.from.LOR(),
						  "RR"=impute.from.LRR())
	
	# calculate counts for each option
	# Option 1:
	a <- res$op1$p1 * N_1; a <- round(a, digits=0);
	b <- N_1
	c <- res$op1$p0 * N_0; c <- round(c, digits=0);
	d <- N_0
	op1 <- list(a=a, b=b, c=c, d=d)
	# Test for valid answers
	if (is.nan(a)|is.nan(b)|is.nan(c)|is.nan(d)) {
		return(list(FAIL=NA))
	}
	
	# Option 2:
	if (isnt.null(res$op2)) {
		a <- res$op2$p1 * N_1; a <- round(a, digits=0);
		b <- N_1                                       
		c <- res$op2$p0 * N_0; c <- round(c, digits=0);
		d <- N_0
		op2 <- list(a=a, b=b, c=c, d=d)
		# Test for valid answers
		if (is.nan(a)|is.nan(b)|is.nan(c)|is.nan(d)) {
			return(list(FAIL=NA))
		}
	}
	else {
		op2 <- NULL;
	}

	if (is.null(op2)) {
		res <- list(op1=op1)
	}
	else {
		res <- list(op1=op1, op2=op2)
	}
	
	
	return(res)
}

#################################################
#                                               #
# Continuous data calculation                   #
# ---                                           #
# The following code is due to Tom Trikalinos.  #
# Originally in fillin.continuous.r file.       #
#                                               #
#################################################
check.1spell.res <- function(n, se) {
    succeeded <- TRUE
    comment <- ""

    if (!is.na(n)) {
        if (n<=1) {
            comment <- "n<=1"
            succeeded <- FALSE
        }        
    }

	if (!is.na(se)) {
	    if (se<=0) {
	        comment <- paste("se<=0", comment, sep=", ")
	        succeeded <- FALSE
		}
	}

    return(list(succeeded=succeeded, comment=comment))

}


########################################################################################
########################################################################################
########################################################################################
########################################################################################
fillin.cont.1spell <- function(n=NA, mean=NA, sd=NA, se=NA, var=NA, 
                         low=NA, high=NA, pval=NA, alpha=0.05) { 
	# var is the SAMPLE variance NOT sampling variance:
	#      var = sd^2 NOT se^2
	#      se = sd/sqrt(n)
    succeeded <- FALSE 
    comment <- ""
    res <- list(succeeded=succeeded)

    z <- abs(qnorm(alpha/2))

    input.vector <- c(n, mean, sd, se, var, low, high, pval)
    input.pattern <- !(is.na(input.vector))
	
	print("Input vector:")
	print(input.vector)
	
	get.mean <- function(high=NA, low=NA) {
		if(is.na(mean))
			mean = (high+low)/2
		return(mean)
	}
	
	get.se <- function(sd=NA, n=NA, low=NA, high=NA, mean=NA, pval=NA) {		
		# try the sd and the n
		if(is.na(se))
			se <- try(  sd/sqrt(n)  , silent=TRUE)
		
		# try both ends of the CI
		if(is.na(se))
			se <- try(  abs(high-low)/(2*z)  ,silent=TRUE)
		
		# try low end of CI
		if(is.na(se))
			se <- try(  abs(mean-low)/z  ,silent=TRUE)
		
		# try high end of CI
		if(is.na(se))
			se <- try(  abs(high-mean)/z  ,silent=TRUE)
		
		# try the 2 sided p-value for the mean != 0
		if(is.na(se))
			se <- try(  mean/abs(qnorm(pval/2))  ,silent=TRUE)
		
		return(se)
	}
	
	get.var <- function(sd=NA) {
		# try sd
		if (is.na(var))
			var <- try(  sd^2  , silent=TRUE)
		return(var)
	}
	
	get.sd <- function(var=NA, n=NA, se=NA) {
		# try var
		if (is.na(sd))
			sd <- try(  sqrt(var)  ,silent=TRUE)
	
		# try se and n
		if (is.na(sd))
			sd <- try(  sqrt(n)*se  ,silent=TRUE)
		
		return(sd)
	}
	
	get.n <- function(sd=NA, se=NA, var=NA) {
		if (is.na(n))
			n <- (sd/se)^2
		if (is.na(n))
			n <- var/(se^2)
		return(n)
	}
	
	dirty <- TRUE
	while (dirty) {
		print("Iterating in fillin.cont1")
		dirty <- FALSE
		
	    ##########################################################
	    # check the mean first 
	    # If not calculate it from the CI
		if (is.na(mean)) {
	    	mean <- get.mean(high=high, low=low)
			if (!is.na(mean)) {
				dirty <- TRUE # mean was changed
				print("changed mean")
			}
				
		}	
	    ##########################################################
	    # if se is missing
		if (is.na(se)) {
			se <- get.se(sd=sd, n=n, low=low, high=high, mean=mean, pval=pval)
			if (!is.na(se)) {
				dirty <- TRUE # se was changed
				print("changed se")
			}
		}
	    ##########################################################
	    # if the SAMPLE variance is missing
		if (is.na(var)) {
			var <- get.var(sd=sd)
			if (!is.na(var)) {
				dirty <- TRUE # var was changed
				print("changed var")
			}
		}
	    ##########################################################
	    # if the lower CI is missing 
	    if(is.na(low)) {
	        low <- mean - z*se
			if (!is.na(low)) {
				dirty <- TRUE # low was changed
				print("changed low")
			}
	    }
	    ##########################################################
	    # if the high CI is missing 
	    if(is.na(high)) {
	        high <- mean + z*se
			if (!is.na(high)) {
				dirty <- TRUE # high was changed
				print("changed high")
			}
	    }
	    ##########################################################
	    # if the 2 sided pval is missing 
	    if(is.na(pval)) {
	        pval <- 2*pnorm(-abs(mean/se))
			if (!is.na(pval)) {
				dirty <- TRUE # pval was changed
				print("changed pval")
			}
	    }
	    ##########################################################
	    # if the sd is missing 
	    if(is.na(sd)) {
	        sd = get.sd(var=var, n=n, se=se)
			if (!is.na(sd)) {
				dirty = TRUE # sd was changed
				print("changed sd")
			}
		}
	    ##########################################################
	    # if the n is missing 
		if(is.na(n)) {
			n <- get.n(sd=sd, se=se, var=var)
			if (!is.na(n)) {
				dirty <- TRUE # sd was changed
				print("changed n")
			}
		}
		
		print("---------------")
		print("Dirty at end:")
		print(dirty)
		print("--------------")
	} # finished iterating
	
	# Do checks:
	if (is.na(mean)) {
		comment <- paste(comment, "no info on mean", sep="|")
		#return(c(res, comment=comment))
	}
	# if the se is still missing, then abort 
	if (is.na(se)) {
		comment <- paste(comment, "no info on dispersion", sep="|")
		#return(c(res, comment=comment))
	}
	if(is.na(sd)) {
		comment <- paste(comment, "{n & sd} missing")
	}
	
    succeeded <- check.1spell.res(n=n, se=se)$succeeded

    output.vector <- c(n, mean, sd, se, var, low, high, pval)
    output.names <- c("n", "mean", "sd", "se", "var", "low", "high", "pval")
    names(output.vector) <- output.names

    res<- list(succeeded=succeeded, input.pattern=input.pattern, output=output.vector, comment=comment)
    return(res)

}


fillin.missing.effect.quantity <- function(est=NA, low=NA, high=NA) {
	# Assumes CI is symmetric around estimate
	
	# low = est - diff, est, high = est + diff
	diff <- high-est
	if (is.na(diff))
		diff <- est - low
	
	if (is.na(est))
		est <- (high-low)/2.0
	
	if (is.na(low))
		low <- est - diff
	
	if (is.na(high))
		high <- est + diff
	
	return(list(est=est, low=low, high=high))
}

gimpute.cont.data <- function(group1, group2, effect_data, conf.level=95.0) {
	# Tries to solve for one of n1,n2, mean1, mean2, sd1, sd2 based on the data
	# in group1, group2, effect_data
	
	# Get 'more' local copies
	n1    <- group1[["n"]]
	n2    <- group2[["n"]]
	mean1 <- group1[["mean"]]
	mean2 <- group2[["mean"]]
	sd1   <- group1[["sd"]]
	sd2   <- group2[["sd"]]
	est   <- effect_data[["est"]]
	low   <- effect_data[["low"]]
	high  <- effect_data[["high"]]
	metric <- effect_data[["metric"]]
	met.param <- effect_data[["met.param"]] # metric specific-parameter
			
	# Convert nulls to NA
	if (is.null(n1))    n1    <- NA
	if (is.null(n2))    n2    <- NA
	if (is.null(mean1)) mean1 <- NA
	if (is.null(mean2)) mean2 <- NA
	if (is.null(sd1))   sd1   <- NA
	if (is.null(sd2))   sd2   <- NA
	if (is.null(est))   est   <- NA
	if (is.null(low))   low   <- NA
	if (is.null(high))  high  <- NA
	if (is.null(metric)) metric <- NA
	if (is.null(met.param)) met.param <- NA
	if (is.null(conf.level)) conf.level <- NA
	
	metric <- as.character(metric)
	
	# Can't do anything if we don't know what metric we are using or if we don't
	# know the conf.level
	if (is.na(metric) | is.na(conf.level) | is.na(met.param)) {
		return(list("FAIL"=NA))
	}
	
	effect.and.ci <- fillin.missing.effect.quantity(est=est, low=low, high=high)
	est  <- effect.and.ci[["est"]]
	low  <- effect.and.ci[["low"]]
	high <- effect.and.ci[["high"]]
	
	# Obtain standard error and variance from CI
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	se <- (high-low)/(2*mult)
	var = se^2
	
	#print("se: "); print(se);
	#print("var: "); print(var);
		
	filter_neg_result <- function(res.vector) {
		# Ignore negative results, complex number results, and condense all NAs to a single one
		res.vector <- res.vector[!is.na(res.vector)]
		res.vector <- res.vector[Re(res.vector) > 0]
		res.vector <- res.vector[abs(Im(res.vector)) < IMAGINARY.THRESHOLD] # imaginary part is very close to zero
		res.vector <- Re(res.vector)
		
		#print("imaginary"); print(Im(res.vector))
		if (length(res.vector)==0)
			res.vector <- NA;
		return(res.vector)
	}
	
	impute.from.MD <- function() {
		print("From MD")
		# Formulas from "The Handbook of Research Synthesis and Meta-Analysis"
	    #     2nd Ed. p. 224
		
		#######################################################################
		# If one of the means is missing, solve for other mean
		#   If we are in here, we already know the effect is mean difference
	    #   D = (mean of group 1) - (mean of group 2)
		D <- est; Y1 <- mean1; Y2 <- mean2;
		
		if (is.na(Y1) & isnt.na(Y2))
			Y1 <- D + Y2
		if (is.na(Y2) & isnt.na(Y1))
			Y2 <- Y1 - D
		#######################################################################
		# For MD, the metric parameter is the assumption that the population SDs
	    # are the same:
		#     met.param == TRUE  # population SDs are the same
	    #     met.param == FALSE # population SDs are not the same
		if (met.param) { # population SDs are the same
			print("Assuming population SDs are the same")
			if (is.na(n1)) {
				#print("n1 is na")
				n1.op1 <- (1/2)*(n2*sd1^2-sd1^2-var*n2^2+2*var*n2+sd2^2*n2-sd2^2+sqrt(var^2*n2^4-4*var^2*n2^3+4*var^2*n2^2+sd1^4+sd2^4+n2^2*sd1^4+2*n2*sd1^4+2*sd1^2*sd2^2+sd2^4*n2^2-2*sd2^4*n2-2*n2^3*sd1^2*var+2*n2^2*sd1^2*var-2*n2^2*sd1^2*sd2^2-4*sd1^2*var*n2+2*var*n2^3*sd2^2+2*var*n2^2*sd2^2-4*var*n2*sd2^2))/(-sd1^2+var*n2)
				n1.op2 <- -(1/2)*(-n2*sd1^2+sd1^2+var*n2^2-2*var*n2-sd2^2*n2+sd2^2+sqrt(var^2*n2^4-4*var^2*n2^3+4*var^2*n2^2+sd1^4+sd2^4+n2^2*sd1^4+2*n2*sd1^4+2*sd1^2*sd2^2+sd2^4*n2^2-2*sd2^4*n2-2*n2^3*sd1^2*var+2*n2^2*sd1^2*var-2*n2^2*sd1^2*sd2^2-4*sd1^2*var*n2+2*var*n2^3*sd2^2+2*var*n2^2*sd2^2-4*var*n2*sd2^2))/(-sd1^2+var*n2)
				print("n1op1"); print(n1.op1);
				print("n1op2"); print(n1.op2);
				n1.op1 <- round(n1.op1, digits = 0)
				n1.op2 <- round(n1.op2, digits = 0)
				n1 <- filter_neg_result(c(n1.op1,n1.op2))
                n1 <- round(n1)
			}
			if (is.na(n2)) {
				#print("n2 is na")
				n2.op1 <- (1/2)*(n1*sd2^2-var*n1^2+2*var*n1+sd1^2*n1-sd1^2-sd2^2+sqrt(sd1^4+sd2^4+2*sd1^2*sd2^2+n1^2*sd2^4+2*n1*sd2^4+var^2*n1^4-4*var^2*n1^3+4*var^2*n1^2+sd1^4*n1^2-2*sd1^4*n1-2*n1^3*sd2^2*var+2*n1^2*sd2^2*var-2*n1^2*sd2^2*sd1^2+2*var*n1^3*sd1^2+2*var*n1^2*sd1^2-4*var*n1*sd1^2-4*var*n1*sd2^2))/(var*n1-sd2^2)
				n2.op2 <- -(1/2)*(-n1*sd2^2+var*n1^2-2*var*n1-sd1^2*n1+sd1^2+sd2^2+sqrt(sd1^4+sd2^4+2*sd1^2*sd2^2+n1^2*sd2^4+2*n1*sd2^4+var^2*n1^4-4*var^2*n1^3+4*var^2*n1^2+sd1^4*n1^2-2*sd1^4*n1-2*n1^3*sd2^2*var+2*n1^2*sd2^2*var-2*n1^2*sd2^2*sd1^2+2*var*n1^3*sd1^2+2*var*n1^2*sd1^2-4*var*n1*sd1^2-4*var*n1*sd2^2))/(var*n1-sd2^2)
				n2.op1 <- round(n2.op1, digits=0)
				n2.op2 <- round(n2.op2, digits=0)
				n2 <- filter_neg_result(c(n2.op1, n2.op2))
                n2 <- round(n2)
			}
			if (is.na(sd1)) {
				#print("sd1 is na")
				sd1.op1 <- sqrt((n1^2-n1+n1*n2-n2)*(var*n1^2*n2+var*n1*n2^2-2*var*n1*n2-n1*sd2^2*n2+n1*sd2^2-sd2^2*n2^2+sd2^2*n2))/(n1^2-n1+n1*n2-n2)
				sd1.op2 <- -sqrt((n1^2-n1+n1*n2-n2)*(var*n1^2*n2+var*n1*n2^2-2*var*n1*n2-n1*sd2^2*n2+n1*sd2^2-sd2^2*n2^2+sd2^2*n2))/(n1^2-n1+n1*n2-n2)
				sd1 <- filter_neg_result(c(sd1.op1, sd1.op2))
			}
			if (is.na(sd2)) {
				#print("sd2 is na")
				sd2.op1 <- sqrt((n1*n2-n1+n2^2-n2)*(var*n1^2*n2+var*n1*n2^2-2*var*n1*n2-sd1^2*n1^2+sd1^2*n1-n2*sd1^2*n1+n2*sd1^2))/(n1*n2-n1+n2^2-n2)
				sd2.op2 <- -sqrt((n1*n2-n1+n2^2-n2)*(var*n1^2*n2+var*n1*n2^2-2*var*n1*n2-sd1^2*n1^2+sd1^2*n1-n2*sd1^2*n1+n2*sd1^2))/(n1*n2-n1+n2^2-n2)
				sd2 <- filter_neg_result(c(sd2.op1, sd2.op2))
			}
		}
		else {  # population SDs are not the same
			print("Not assuming population SDs are the same")
			if (is.na(n1)) {
				#print("n1 is na")
				n1 <- n2*sd1^2/(var*n2-sd2^2)
                n1 <- round(n1)
			}
			if (is.na(n2)) {
				#print("n2 is na")
				n2 <- n1*sd2^2/(var*n1-sd1^2)
                n2 <- round(n2)
			}
			if (is.na(sd1)) {
				#print("sd1 is na")
				sd1.op1 <- sqrt(n2*n1*(var*n2-sd2^2))/n2
				sd1.op2 <- -sqrt(n2*n1*(var*n2-sd2^2))/n2
				sd1 <- filter_neg_result(c(sd1.op1, sd1.op2))
			}
			if (is.na(sd2)) {
				#print("sd2 is na")
				sd2.op1 <- sqrt(n1*n2*(var*n1-sd1^2))/n1
				sd2.op2 <- -sqrt(n1*n2*(var*n1-sd1^2))/n1
				sd2 <- filter_neg_result(c(sd2.op1, sd2.op2))
			}
		}
		
		res <- list(n1=n1, n2=n2, mean1=Y1, mean2=Y2, sd1=sd1, sd2=sd2)
		return(res)
	} # end of impute.from.MD
	
	impute.from.SMD <- function() {
		print("From SMD")
		#######################################################################
		# If one of the means is missing	
		sdw <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)) # within-groups sd
		D <- est; Y1 <- mean1; Y2 <- mean2;
		
		if (is.na(Y1)) Y1 <- D*sdw+Y2
		if (is.na(Y2)) Y2 <- -D*sdw+Y1
		#######################################################################
		# First try some stuff that does not depend on the metric parameter
		# From formula: d=(Y1-Y2)/SW, SW^2=((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)
		if (is.na(n1)) {
			n1 <- -(-sd1^2*D^2+sd2^2*n2*D^2-sd2^2*D^2-n2*Y1^2+2*n2*Y1*Y2-n2*Y2^2+2*Y1^2-4*Y1*Y2+2*Y2^2)/(sd1^2*D^2-Y1^2+2*Y1*Y2-Y2^2)
            n1 <- round(n1)
		}
		if (is.na(n2)) {
			n2 <- -(sd1^2*n1*D^2-sd1^2*D^2-sd2^2*D^2-n1*Y1^2+2*n1*Y1*Y2-n1*Y2^2+2*Y1^2-4*Y1*Y2+2*Y2^2)/(sd2^2*D^2-Y1^2+2*Y1*Y2-Y2^2)
            n2 <- round(n2)
		}
		if (is.na(sd1)) {
			sd1.op1 <- (sqrt(-(n1-1)*(-n1*Y1^2+2*n1*Y1*Y2+sd2^2*n2*D^2-sd2^2*D^2+2*n2*Y1*Y2-n2*Y2^2-n1*Y2^2-n2*Y1^2+2*Y2^2+2*Y1^2-4*Y1*Y2)))/((n1-1)*D)
			sd1.op2 <- -(sqrt(-(n1-1)*(-n1*Y1^2+2*n1*Y1*Y2+sd2^2*n2*D^2-sd2^2*D^2+2*n2*Y1*Y2-n2*Y2^2-n1*Y2^2-n2*Y1^2+2*Y2^2+2*Y1^2-4*Y1*Y2)))/((n1-1)*D)
			sd1 <- filter_neg_result(c(sd1.op1, sd1.op2))
		}
		if (is.na(sd2)) {
			sd2.op1 <- (sqrt(-(n2-1)*(sd1^2*n1*D^2-sd1^2*D^2-n1*Y2^2-n2*Y1^2-n1*Y1^2+2*n1*Y1*Y2+2*Y1^2-4*Y1*Y2+2*n2*Y1*Y2-n2*Y2^2+2*Y2^2)))/((n2-1)*D)
			sd2.op2 <- -(sqrt(-(n2-1)*(sd1^2*n1*D^2-sd1^2*D^2-n1*Y2^2-n2*Y1^2-n1*Y1^2+2*n1*Y1*Y2+2*Y1^2-4*Y1*Y2+2*n2*Y1*Y2-n2*Y2^2+2*Y2^2)))/((n2-1)*D)
			sd2 <- filter_neg_result(c(sd2.op1, sd2.op2))
		}
		#######################################################################
		# For SMD, the metric parameter is whether hedges g is used
		#     met.param == TRUE  # SMD is Hedges' g (corrected bias) (default)
	    #     met.param == FALSE # SMD has uncorrected bias
		if (met.param) { # using Hedges' g
			print("Assuming SMD is Hedges' g")
			if (is.na(n1)) {
				tryCatch({n1 <- polyroot(c(96*n2^3-16*n2^4-144*n2^2, (81*var*n2^2-72*var*n2^3+48*D^2*n2^2-72*D^2*n2+16*var*n2^4-288*n2-64*n2^3+288*n2^2-8*n2^3*D^2), (48*D^2*n2+48*var*n2^3+288*n2-16*D^2*n2^2-144-144*var*n2^2+81*var*n2-96*n2^2), (96+48*var*n2^2-64*n2-8*D^2*n2-72*var*n2), (16*var*n2-16)));
					}, error = function(e) {
						#print(e);
						n1 <- NA;
					});
				n1 <- filter_neg_result(n1)
        		n1 <- round(n1)
			}
			if (is.na(n2)) {
				tryCatch({  n2 <- polyroot(c(96*n1^3-16*n1^4-144*n1^2, (81*var*n1^2-72*var*n1^3+48*D^2*n1^2-72*D^2*n1+16*var*n1^4-288*n1-64*n1^3+288*n1^2-8*n1^3*D^2), (48*D^2*n1+48*var*n1^3+288*n1-16*D^2*n1^2-144-144*var*n1^2+81*var*n1-96*n1^2), (96+48*var*n1^2-64*n1-8*D^2*n1-72*var*n1), (16*var*n1-16)));
					}, error = function(e) {
						#print(e);
						n2 <- NA;
					});
				n2 <- filter_neg_result(n2)
       			n2 <- round(n2)
			}
		}
		else { # not using Hedges' g
			if (is.na(n1)) {
				n1.op1 <- (1/4)*(-2*var*n2+4+D^2+sqrt(4*var^2*n2^2-4*D^2*n2*var+8*D^2+D^4))*n2/(var*n2-1)
				n1.op2 <- -(1/4)*(2*var*n2-4-D^2+sqrt(4*var^2*n2^2-4*D^2*n2*var+8*D^2+D^4))*n2/(var*n2-1)
				n1.op1 <- round(n1.op1, digits = 0)
				n1.op2 <- round(n1.op2, digits = 0)
				n1 <- filter_neg_result(c(n1.op1,n1.op2))
                n1 <- round(n1)
			}
			if (is.na(n2)) {
				n2.op1 <- (1/4)*(-2*var*n1+D^2+4+sqrt(4*var^2*n1^2-4*var*n1*D^2+D^4+8*D^2))*n1/(-1+var*n1)
				n2.op2 <- -(1/4)*(2*var*n1-D^2-4+sqrt(4*var^2*n1^2-4*var*n1*D^2+D^4+8*D^2))*n1/(-1+var*n1)
				n2.op1 <- round(n2.op1, digits=0)
				n2.op2 <- round(n2.op2, digits=0)
				n2 <- filter_neg_result(c(n2.op1, n2.op2))
                n2 <- round(n2)
			}
		}
		
		res <- list(n1=n1, n2=n2, mean1=Y1, mean2=Y2, sd1=sd1, sd2=sd2)
		return(res)
	} # end of impute.from.smd
	
	res <- switch(metric, "MD"=impute.from.MD(), "SMD"=impute.from.SMD())
	return(res)
}

########################################################################################
# Tom goes a bit overboard with the '#'s :) #########################################################
########################################################################################
########################################################################################
fillin.cont.AminusB <- function(
    n.A=NA, mean.A=NA, sd.A=NA, se.A=NA, var.A=NA, low.A=NA, high.A=NA, pval.A=NA, 
    n.B=NA, mean.B=NA, sd.B=NA, se.B=NA, var.B=NA, low.B=NA, high.B=NA, pval.B=NA,
    correlation = 0, alpha=0.05) {


    succeeded <- TRUE  
    comment <- ""
    res <- list(succeeded= succeeded)
    
    #######
    # anything that's returned needs to be initialized to NA here
    #
    n.diff <- NA
    mean.diff <- NA
    sd.diff <- NA
    se.diff <- NA
    var.diff <- NA
    low.diff <- NA
    high.diff <- NA
    pval.diff <-NA
    
    z <- abs(qnorm(alpha/2))

    input.vector.A <- c(n.A, mean.A, sd.A, se.A, var.A, low.A, high.A, pval.A) 
    input.vector.B <- c(n.B, mean.B, sd.B, se.B, var.B, low.B, high.B, pval.B)
    input.pattern <- list(A=!(is.na(input.vector.A)), B=!(is.na(input.vector.B)))

    fillin.A <- fillin.cont.1spell(n.A, mean.A, sd.A, se.A, var.A, low.A, high.A, pval.A, alpha=alpha)
    comment <-paste(comment, paste("A", fillin.A$comment, sep=":"), sep="|")

    fillin.B <- fillin.cont.1spell(n.B, mean.B, sd.B, se.B, var.B, low.B, high.B, pval.B, alpha=alpha)
    comment <-paste(comment, paste("B", fillin.B$comment, sep=":"), sep="|")

    # you do not need to tryCatch here
    if (identical( c(fillin.A$succeeded,fillin.B$succeeded), c(TRUE, TRUE))) {

        mean.diff <- fillin.A$output["mean"] - fillin.B$output["mean"]
        var.diff  <- (fillin.A$output["se"])^2 + (fillin.B$output["se"])^2 
                     - 2*correlation*(fillin.A$output["se"])*(fillin.B$output["se"])

        se.diff   <- sqrt(var.diff)
        low.diff  <- mean.diff - z * se.diff
        high.diff <- mean.diff + z * se.diff
        pval.diff  <- 2*pnorm(-abs(mean.diff/se.diff))

        # these are not of real interest
        n.diff  <- try(min(fillin.A$output["n"], fillin.B$output["n"]), silent=TRUE)
        sd.diff <- try(var.diff * (n.diff - 1) , silent=TRUE)
    } 
    else {
        succeeded <- FALSE 
    }

    output.vector <- c(n.diff, mean.diff, sd.diff, se.diff, var.diff, 
                      low.diff, high.diff, pval.diff)
    output.names <- c("n", "mean", "sd", "se", "var", "low", "high", "pval")
    names(output.vector) <- output.names
    #names(fillin.A) <- output.names
    #names(fillin.B) <- output.names
    res<- list(succeeded=succeeded, input.pattern=input.pattern, output=output.vector, 
                      pre=fillin.A$output, post=fillin.B$output,
                      comment=comment, correlation=correlation)

    return(res)

}

################################ 
# Diagnostic data calculation  #
################################
gimpute.diagnostic.data <- function(diag.data) {
	# imputes diagnostic data (2x2 tables) from fields in diag.data data frame
	# paramater. This will include (perhaps):
	#   TP, FN, TN, FP, N, prev, sens, sens.lb, sens.ub, spec, spec.lb, spec.ub,
	#   alpha
	# Ignore the case #s below, just a way i was working things out
	
	#initialize local variables
	
	TP <- NULL; FN <- NULL; TN <- NULL; FP <-NULL;
			
	N    <-       diag.data[["total"]]
	prev <-       diag.data[["prev"]]
	sens <-       diag.data[["sens"]]
	sens.lb <-    diag.data[["sens.lb"]]
	sens.ub <-    diag.data[["sens.ub"]]
	spec    <-    diag.data[["spec"]]
	spec.lb <-    diag.data[["spec.lb"]]
	spec.ub <-    diag.data[["spec.ub"]]
	conf.level <- diag.data[["conf.level"]]
	
	case2a.condition <- isnt.null(sens) & isnt.null(prev) & isnt.null(N)
	case2b.condition <- isnt.null(spec) & isnt.null(prev) & isnt.null(N)
	
	tmpA <- isnt.null(sens) & (isnt.null(sens.lb) | isnt.null(sens.ub))
	tmpB <- isnt.null(sens.lb) & isnt.null(sens.ub)
	case5a.condition <- (tmpA | tmpB) & isnt.null(conf.level)
	case5b.condition <- case5a.condition & isnt.null(spec) & isnt.null(N)
	
	tmpA <- isnt.null(spec) & (isnt.null(spec.lb) | isnt.null(spec.ub))
	tmpB <- isnt.null(spec.lb) & isnt.null(spec.ub)
	case6a.condition <- (tmpA | tmpB) & isnt.null(conf.level)
	case6b.condition <- case6a.condition & isnt.null(sens) & isnt.null(N)
	
	tmpA <- isnt.null(sens) & (isnt.null(sens.lb) | isnt.null(sens.ub))
	tmpA <- tmpA | (isnt.null(sens.lb) & isnt.null(sens.ub))
	tmpB <- isnt.null(spec) & (isnt.null(spec.lb) | isnt.null(spec.ub))
	tmpB <- tmpB | (isnt.null(spec.lb) & isnt.null(spec.ub))
	case8a.condition <- tmpA & isnt.null(conf.level)
	case8b.condition <- tmpB & isnt.null(conf.level)
	
	# Case 2: inputs: sens, spec, prev, N
	case2 <- function(sens, spec, prev, N) {
		TP <- sens*prev*N
		FN <- (1-sens)*prev*N
		FP <- N*(spec-1)*(prev-1)
		TN <- N*spec*(1-prev)
		
		list(TP=TP,FP=FP,TN=TN,FN=FN)
	}
	# Case 5: inputs: sens, sens.lb or sens.ub, spec, N, conf.level
	case5 <- function(sens, sens.lb, sens.ub, spec, N, conf.level) {
		ci.data <- list(estimate=sens, lb=sens.lb, ub=sens.ub, conf.level=conf.level)
		est.var <- calc.est.var(ci.data)
		varLogitSENS <- est.var$var
		sens <- est.var$estimate
		
		TP = -1/(varLogitSENS*(sens-1))
		FP = -(-1+spec)*(varLogitSENS*sens^2*N-varLogitSENS*sens*N+1)/(varLogitSENS*sens*(sens-1))
		TN = spec*(varLogitSENS*sens^2*N-varLogitSENS*sens*N+1)/(varLogitSENS*sens*(sens-1))
		FN = 1/(varLogitSENS*sens)
		
		list(TP=TP,FP=FP,TN=TN,FN=FN)
	}
	
	
	# Case 6: inputs: spec, spec.lb or spec.ub, sens, N, conf.level
	case6 <- function(spec, spec.lb, spec.ub, sens, N, conf.level) {
		ci.data <- list(estimate=spec, lb=spec.lb, ub=spec.ub, conf.level=conf.level)
		est.var <- calc.est.var(ci.data)
		varLogitSPEC <- est.var$var
		spec <- est.var$estimate
		
		TP = sens*(-1*varLogitSPEC*spec*N+varLogitSPEC*spec^2*N+1)/(varLogitSPEC*spec*(-1+spec))
		FP = 1/(varLogitSPEC*spec)
		TN = -1/(varLogitSPEC*(-1+spec))
		FN = -(sens-1)*(-1*varLogitSPEC*spec*N+varLogitSPEC*spec^2*N+1)/(varLogitSPEC*spec*(-1+spec))
		
		list(TP=TP,FP=FP,TN=TN,FN=FN)
	}
	
	# Case 8: inputs sens, sens.lb or sens.ub, spec, spec.lb or spec.ub, conf.level
	case8 <- function(sens, sens.lb, sens.ub, spec, spec.lb, spec.ub, conf.level) {
		ci.data <- list(estimate=sens, lb=sens.lb, ub=sens.ub, conf.level=conf.level)
		est.var <- calc.est.var(ci.data)
		varLogitSENS <- est.var$var
		sens <- est.var$estimate
		
		ci.data <- list(estimate=spec, lb=spec.lb, ub=spec.ub, conf.level=conf.level)
		est.var <- calc.est.var(ci.data)
		varLogitSPEC <- est.var$var
		spec <- est.var$estimate
		
		TP = -1/(varLogitSENS*(sens-1))
		FP = 1/(varLogitSPEC*spec)
		TN = -1/(varLogitSPEC*(-1+spec))
		FN = 1/(varLogitSENS*sens)
	
		list(TP=TP,FP=FP,TN=TN,FN=FN)
	}


	
	case2res <- case2(sens, spec, prev, N)
	case5res <- case5(sens, sens.lb, sens.ub, spec, N, conf.level)
    case6res <- case6(spec, spec.lb, spec.ub, sens, N, conf.level)
	case8res <- case8(sens, sens.lb, sens.ub, spec, spec.lb, spec.ub, conf.level)

	# TP,FN
	if (case2a.condition) {
		print("Entering 2a")
		TP <- if(is.null(TP)) case2res$TP
		FN <- if(is.null(FN)) case2res$FN
	} else if (case5a.condition) {
		print("Entering 5a")
		TP <- if(is.null(TP)) case5res$TP
		FN <- if(is.null(FN)) case5res$FN
	} else if (case6b.condition) {
		print("Entering 6b")
		TP <- if(is.null(TP)) case6res$TP
		FN <- if(is.null(FN)) case6res$FN
	} else if (case8a.condition) {
		print("Entering 8a")
		TP <- if(is.null(TP)) case8res$TP
		FN <- if(is.null(FN)) case8res$FN
	}
	
	# TN,FP
	if (case2b.condition) {
		print("Entering 2b")
		TN <- if(is.null(TN)) case2res$TN
	    FP <- if(is.null(FP)) case2res$FP
	} else if (case5b.condition) {
		print("Entering 5b")
		TN <- if(is.null(TN)) case5res$TN
		FP <- if(is.null(FP)) case5res$FP
	} else if (case6a.condition) {
		print("Entering 6a")
		TN <- if(is.null(TN)) case6res$TN
		FP <- if(is.null(FP)) case6res$FP
	} else if (case8b.condition) {
		print("Entering 8b")
		TN <- if(is.null(TN)) case8res$TN
		FP <- if(is.null(FP)) case8res$FP
	}
	
	# Convert NULL to NA for fun, also other reasons?
	if(is.null(TP)) {
    	TP <- NA
	}
	if(is.null(FN)) {
    	FN <- NA
	}
	if(is.null(TN)) {
    	TN <- NA
	}
	if(is.null(FP)) {
    	FP <- NA
	}
	
	# calculate rounding error
	TP.rnd.err <- abs(TP-round(TP,digits=0))
	FN.rnd.err <- abs(FN-round(FN,digits=0))
	TN.rnd.err <- abs(TN-round(TN,digits=0))
	FP.rnd.err <- abs(FP-round(FP,digits=0))
	
	TP <- round(TP,digits=0)
	FN <- round(FN,digits=0)
	TN <- round(TN,digits=0)
	FP <- round(FP,digits=0)
	
	# return
	list(TP=TP,
		 FN=FN,
		 TN=TN,
		 FP=FP,
		 TP.rnd.err=TP.rnd.err,
		 FN.rnd.err=FN.rnd.err,
		 TN.rnd.err=TN.rnd.err,
		 FP.rnd.err=FP.rnd.err)
}

calc.est.var <- function(ci.data) {
  # calculate estimate and variance given any two of the following: estimate, ci lower bound, ci upper bound.
  #
  est.var <- list()
  alpha <- 1.0-(ci.data$conf.level/100.0)
  mult <- abs(qnorm(alpha/2.0))
  if (isnt.null(ci.data$estimate)) {
    # if estimate is there, use it.
    if (isnt.null(ci.data$lb)) {
    est.var$estimate <- ci.data$estimate
    var <- ((logit(ci.data$estimate) - logit(ci.data$lb)) / mult)^2
    est.var$var <- var
    } else if (isnt.null(ci.data$ub)) {
    est.var$estimate <- ci.data$estimate
    var <- ((logit(ci.data$ub) - logit(ci.data$estimate)) / mult)^2
    est.var$var <- var
    }
  } else if (isnt.null(ci.data$lb) & isnt.null(ci.data$ub)) {
    # estimate isn't there.
    radius <- (logit(ci.data$ub) - logit(ci.data$lb)) / 2
    estimate <- invlogit(logit(ci.data$lb) + radius)
    est.var$estimate <- estimate
    var <- (radius / mult)^2
    est.var$var <- var
  }
  est.var
}

rescale.effect.and.ci.conf.level <- function(dataf.arg) {
	# Rescales est,low,high to target confidence level
	#  dataf.arg is a dataframe of arguments
	
	# est, low, high are assumed to be on the calc scale
	# returns rescaled est,low,high also on calc scale
	est = dataf.arg[["est"]]
	low = dataf.arg[["low"]]
	high = dataf.arg[["high"]] 
	orig.conf.level = dataf.arg[["orig.conf.level"]]
	target.conf.level = dataf.arg[["target.conf.level"]]
	
	# Convert NULL to NA
	if (is.null(est))  est  <- NA
	if (is.null(low))  low  <- NA
	if (is.null(high)) high <- NA
	
	# make sure we have the right inputs
	num_na = 0
	if (is.na(est)) {num_na = num_na + 1}
	if (is.na(low)) {num_na = num_na + 1}
	if (is.na(high)) {num_na = num_na + 1}
	
	if ((num_na > 1) || is.na(orig.conf.level) || is.na(target.conf.level)) {
		return(list("FAIL"=NA)) # failure
	}
	
	# make sure est, low, high are all not NA
	if (is.na(est)) {
		est <- (high-low)/2.0
	}

	if (is.na(low)) {
		low <- est - (high-est)
	}

	if (is.na(high)) {
		high <- est + (est - low)	
	}

	old.alpha <- 1.0-(orig.conf.level/100.0)
	new.alpha <- 1.0-(target.conf.level/100.0)
	old.mult <- abs(qnorm(old.alpha/2.0))
	new.mult <- abs(qnorm(new.alpha/2.0))
	
	se <- (high-low)/(2*old.mult)
	
	new.est  <- est
	new.low  <- new.est - new.mult*se
	new.high <- new.est + new.mult*se
	
	return(list(est=new.est, low=new.low, high=new.high))
}



########### OLD DIAGNOSTIC FUNCTIONS NOT CURRENTLY USED ########################
# Do not delete them yet, they are good for reference #

#impute.diagnostic.data <- function(diag.data, metric){
#    # this function imputes diagnostic data (i.e., 2x2 tables) from the fields
#    # available in the diagnostic.data data frame parameter.
#    #
#    if (metric=="Sens") {
#      ci.data <- list(estimate=diag.data$sens, lb=diag.data$sens.lb, ub=diag.data$sens.ub, conf.level=diag.data$conf.level)
#      est.var <- calc.est.var(ci.data)
#      # fill in estimate, ci.lb, ci.ub
#      diag.data$sens <- est.var$estimate
#      diag.data$sens.var <- est.var$var
#      diag.data <- calc.sens.data(diag.data)
#    } else if (metric=="Spec") {
#      ci.data <- list(estimate=diag.data$spec, lb=diag.data$spec.lb, ub=diag.data$spec.ub, conf.level=diag.data$conf.level)
#      est.var <- calc.est.var(ci.data)
#      # fill in estimate, ci.lb, ci.ub
#      diag.data$spec <- est.var$estimate
#      diag.data$spec.var <- est.var$var
#      diag.data <- calc.spec.data(diag.data)
#    }  
#    diag.data 
#}

#calc.sens.data <- function(diag.data) {
#  # back-calculate TP, FN, sens, var from any two known values
#  # Notes:  TP = 1 / (( 1-sens) * var)
#  #         FN = 1 / (sens * var)
#  #         TP / FN = sens / (1-sens)
#  #         FN / TP = (1-sens) / sens
#  #
#  TP<-NULL; FN<-NULL; TN<-NULL; FP<-NULL; sens<-NULL; sens.var<-NULL
#  
#    
#  if (isnt.null(diag.data$sens) & isnt.null(diag.data$sens.var)) {
#    sens <- diag.data$sens
#    sens.var <- diag.data$sens.var
#    diag.data$TP <- round(1 / ((1-sens) * sens.var), digits=0)
#    diag.data$FN <- round(1 / (sens * sens.var), digits=0)
#  } else if (isnt.null(diag.data$sens) & isnt.null(diag.data$TP)) {
#    sens <- diag.data$sens 
#    TP <- diag.data$TP
#    diag.data$FN <- round(((1 - sens) / sens) * TP, digits=0)
#    diag.data$sens.var <- 1 / ((1 - sens) * TP)
#  } else if (isnt.null(diag.data$sens) & isnt.null(diag.data$FN)) {
#    sens <- diag.data$sens 
#    FN <- diag.data$FN
#    diag.data$TP <- round((sens / (1 - sens)) * FN, digits=0)
#    diag.data$sens.var <- 1 / (sens * FN)
#  } else if (isnt.null(diag.data$sens.var) & isnt.null(diag.data$TP)) {
#    sens.var <- diag.data$sens.var 
#    TP <- diag.data$TP
#    sens <- 1 - 1/(TP * sens.var)
#    diag.data$sens <- sens
#    diag.data$FN <- round(1 / (sens * sens.var), digits=0)
#  } else if (isnt.null(diag.data$sens.var) & isnt.null(diag.data$FN)) {
#    sens.var <- diag.data$sens.var 
#    FN <- diag.data$FN
#    sens <- 1 / (FN * sens.var)
#    diag.data$sens <- sens
#    diag.data$TP <- round(1 / ((1-sens) * sens.var), digits=0)
#  }     
#  diag.data
#}
#
#calc.spec.data <- function(diag.data) {
#  # back-calculate TN, FP, spec, var from any two known values
#  # Note: This function is identical to calc.sens.data with the following substitutions:
#  # sens -> spec
#  # TP -> TN
#  # FN -> FP
#  # Could combine into one function, but it would be less readable. 
#  #
#  # Notes:  TN = 1 / (( 1-spec) * var)
#  #         FP = 1 / (spec * var)
#  #         TN / FP = spec / (1-spec)
#  #         FP / TN = (1-spec) / spec
#  #
#  TN<-NULL; FP<-NULL; TN<-NULL; FP<-NULL; spec<-NULL; spec.var<-NULL
#  
#    
#  if (isnt.null(diag.data$spec) & isnt.null(diag.data$spec.var)) {
#    spec <- diag.data$spec
#    spec.var <- diag.data$spec.var
#    diag.data$TN <- round(1 / ((1-spec) * spec.var), digits=0)
#    diag.data$FP <- round(1 / (spec * spec.var), digits=0)
#  } else if (isnt.null(diag.data$spec) & isnt.null(diag.data$TN)) {
#    spec <- diag.data$spec 
#    TN <- diag.data$TN
#    diag.data$FP <- round(((1 - spec) / spec) * TN, digits=0)
#    diag.data$spec.var <- 1 / ((1 - spec) * TN)
#  } else if (isnt.null(diag.data$spec) & isnt.null(diag.data$FP)) {
#    spec <- diag.data$spec 
#    FP <- diag.data$FP
#    diag.data$TN <- round((spec / (1 - spec)) * FP, digits=0)
#    diag.data$spec.var <- 1 / (spec * FP)
#  } else if (isnt.null(diag.data$spec.var) & isnt.null(diag.data$TN)) {
#    spec.var <- diag.data$spec.var 
#    TN <- diag.data$TN
#    spec <- 1 - 1/(TN * spec.var)
#    diag.data$spec <- spec
#    diag.data$FP <- round(1 / (spec * spec.var), digits=0)
#  } else if (isnt.null(diag.data$spec.var) & isnt.null(diag.data$FP)) {
#    spec.var <- diag.data$spec.var 
#    FP <- diag.data$FP
#    spec <- 1 / (FP * spec.var)
#    diag.data$spec <- spec
#    diag.data$TN <- round(1 / ((1-spec) * spec.var), digits=0)
#  }          
#  diag.data
#}