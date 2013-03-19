isnt.null <- function(x){
    # some syntactic sugar.
    ! is.null(x)    
}


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
	
#	print("Bin data in R:")
#	print(bin.data)
	
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
	
	
	
	###############################
#	print("just metric:")
#	print(metric)
#	cat("Metric",metric,
#		"\nest",est,
#		"\nlower",lower,
#		"\nupper",upper,
#		"\nN_1",N_1,
#		"\nN_0",N_0,
#		"Conf.level",conf.level)
	###############################
	
	alpha <- 1.0-(conf.level/100.0)
	mult <- abs(qnorm(alpha/2.0)) # 1.96 for 95% CI
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
		
		#print("d: ")
		#print(d)
		#print("b: "); print(b)
		
		d <- exp(d) # convert OR back to normal scale (not log)
		
		A <- N_0*(1-d)^2+b*d*N_0*N_1
		B <- -1*(2*N_0*(1-d)+b*d*N_0*N_1)
		C <- N_0 + d*N_1
		
		#print("A: "); print(A);
		#print("B: "); print(B);
		#print("C: "); print(C);
		
		# calculate proportions
		p0.op1 <- (-B+sqrt(B^2-4*A*C))/(2*A) 
		p0.op2 <- (-B-sqrt(B^2-4*A*C))/(2*A) 
		p1.op1 <- d*p0.op1/(d*p0.op1+1-p0.op1)
		p1.op2 <- d*p0.op2/(d*p0.op2+1-p0.op2)
		
		#print("p0"); print(p0.op1); print(p0.op2);
		#print("p1"); print(p1.op1); print(p1.op2);
		
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
		
		#print("p0"); print(p0.op1);
		#print("p1"); print(p1.op1); 
		
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

	res <- list(op1=op1, op2=op2)
}
	
	


#### WTF??? #####
impute.bin.data <- function(bin.data){
    # this function imputes binary data (i.e., 2x2 tables) from the fields
    # available in the bin.data data frame parameter.
    #
    # a,b,c and d are the respective entries for the 2-x-2 table. they denote 
    # treated events, treated total, control events, and control total, respectively
    a<-NULL; b<-NULL; c<-NULL; d<-NULL;
    
	if (is.null(bin.data$estimate)){
	    # these are the trivial cases; everything was given in nearly the form we want
	    if (isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$control.n.no.outcome) & 
	        isnt.null(bin.data$tx.n.outcome) & isnt.null(bin.data$tx.n.no.outcome)) {
	            a <- bin.data$tx.n.outcome
	            b <- bin.data$tx.n.outcome + bin.data$tx.n.no.outcome
	            c <- bin.data$control.n.outcome
	            d <- bin.data$control.n.outcome+bin.data$control.n.no.outcome
	    }
	    else if (isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$control.N) &
	               isnt.null(bin.data$tx.n.outcome) & isnt.null(bin.data$tx.N)) {
	            a <- bin.data$tx.n.outcome
	            b <- bin.data$tx.N
	            c <- bin.data$control.n.outcome
	            d <- bin.data$control.N                 
	    }
	    else if (isnt.null(bin.data$control.p.outcome) & isnt.null(bin.data$control.N) &
	               isnt.null(bin.data$tx.p.outcome) & isnt.null(bin.data$tx.N)){
	            a <- bin.data$tx.p.outcome * bin.data$tx.N
	            b <- bin.data$tx.N
	            c <- bin.data$control.p.outcome * bin.data$control.N
	            d <- bin.data$control.N
	    }           
	}
    else{
        if (isnt.null(bin.data$estimate) & isnt.null(bin.data$l_ci) & isnt.null(bin.data$u_ci) &
            isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)){
                a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2.0
                b <- log(bin.data$estimate) 
                c <- sqrt(a) * (ln(bin.data$u_ci) - b)/1.96 # TODO parameterize
                # no d/x4 here? (total control)
        }
        else if(isnt.null(bin.data$estimate) & isnt.null(bin.data$pval) & 
                  isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)){
                a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2.0
                b <- log(bin.data$estimate)
                c <- b * sqrt(a) / qnorm(bin.data$pval/2.0)
        }
        else if(isnt.null(bin.data$log.estimate) & isnt.null(bin.data$log.se) & 
                  isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)){
                a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2.0
                b <- bin.data$log.estimate
                c <- bin.data$log.se * sqrt(a)
        }
        else if(isnt.null(bin.data$log_estimate) & isnt.null(bin.data$pval) &
                  isnt.null(bin.data$control.n.outcome) & isnt.null(bin.data$tx.n.outcome)){
                a <- (bin.data$control.n.outcome + bin.data$tx.n.outcome)/2.0 
                b <- log(bin.data$log.estimate)
                c <- b * sqrt(a) / qnorm(bin.data$pval/2.0)
        }
    }
    data.frame(a=a, b=b, c=c, d=d)
}

# Not currently being used but its so cool nonetheless so don't get rid of it
#fillin.2x2.simple <- function(c11=NA, c12=NA, c21=NA, c22=NA, 
#                                                r1sum =NA, r2sum=NA, 
#                                                c1sum=NA, c2sum=NA,
#                                                total=NA, touse=rep(TRUE,9)){
#	y <- c(
#	rep(c11,4), 
#	rep(c12,4),
#	rep(c21,4), 
#	res<-rep(c22,4),
#	rep(r1sum,3), 
#	rep(r2sum,3), 
#	rep(c1sum,3), 
#	rep(c2sum,3),
#	rep(total,8)) 
#
#    select <- c(
#		rep(touse[1],4), 
#		rep(touse[2],4), 
#		rep(touse[3],4), 
#		rep(touse[4],4), 
#		rep(touse[5],3), 
#		rep(touse[6],3), 
#		rep(touse[7],3), 
#		rep(touse[8],3), 
#		rep(touse[9],8))
#
#	X<- c(
#	# c11, c12, c21, c22, r1sum, r2sum, c1sum, c2sum, total 
#	    1,   0,   0,   0,     0,     0,     0,     0,     0,  #c11
#	    0,  -1,   0,   0,     1,     0,     0,     0,     0,  #c11=r1sum-c12
#	    0,   0,  -1,   0,     0,     0,     1,     0,     0,  #c11=c1sum-c21
#	    0,  -1,  -1,  -1,     0,     0,     0,     0,     1,  #c11=total-c12-c21-c22
#
#	    0,   1,   0,   0,     0,     0,     0,     0,     0,  #c12
#	   -1,   0,   0,   0,     1,     0,     0,     0,     0,  #c12=r1sum-c11
#	    0,   0,   0,  -1,     0,     0,     0,     1,     0,  #c12=c2sum-c22
#	   -1,   0,  -1,  -1,     0,     0,     0,     0,     1,  #c12=total-c11-c21-c22
#
#	    0,   0,   1,   0,     0,     0,     0,     0,     0,  #c21
#	    0,   0,   0,  -1,     0,     1,     0,     0,     0,  #c21=r2sum-c22
#	   -1,   0,   0,   0,     0,     0,     1,     0,     0,  #c21=c1sum-c11
#	   -1,  -1,   0,  -1,     0,     0,     0,     0,     1,  #c21=total-c11-c12-c22
#
#	    0,   0,   0,   1,     0,     0,     0,     0,     0,  #c22
#	    0,   0,  -1,   0,     0,     1,     0,     0,     0,  #c22=r2sum-c21
#	    0,  -1,   0,   0,     0,     0,     0,     1,     0,  #c22=c2sum-c12
#	   -1,  -1,  -1,   0,     0,     0,     0,     0,     1,  #c22=total-c11-c12-c21
#	
#	    0,   0,   0,   0,     1,     0,     0,     0,     0,  #r1sum
#	    1,   1,   0,   0,     0,     0,     0,     0,     0,  #r1sum=c11+c12
#	    0,   0,   0,   0,     0,    -1,     0,     0,     1,  #r1sum=total-r2sum
#
#	    0,   0,   0,   0,     0,     1,     0,     0,     0,  #r2sum
#	    0,   0,   1,   1,     0,     0,     0,     0,     0,  #r2sum=c21+c22
#	    0,   0,   0,   0,    -1,     0,     0,     0,     1,  #r2sum=total-r1sum
#	    
#	    0,   0,   0,   0,     0,     0,     1,     0,     0,  #c1sum
#	    1,   0,   1,   0,     0,     0,     0,     0,     0,  #c1sum=c11+c21
#	    0,   0,   0,   0,     0,     0,     0,    -1,     1,  #c1sum=total-c2sum
#
#	    0,   0,   0,   0,     0,     0,     0,     1,     0,  #c2sum
#	    0,   1,   0,   1,     0,     0,     0,     0,     0,  #c2sum=c12+c22
#	    0,   0,   0,   0,     0,     0,    -1,     0,     1,  #c2sum=total-c1sum
#
#	    0,   0,   0,   0,     0,     0,     0,     0,     1,  #total
#	    1,   1,   1,   1,     0,     0,     0,     0,     0,  #total=c11+c12+c21+c22
#	    0,   0,   0,   0,     1,     1,     0,     0,     0,  #total=r1sum+r2sum
#	    0,   0,   0,   0,     0,     0,     1,     1,     0,  #total=c1sum+c2sum
#	    0,   0,   1,   1,     1,     0,     0,     0,     0,  #total=r1sum+c21+c22
#	    1,   1,   0,   0,     0,     1,     0,     0,     0,  #total=r2sum+c11+c12
#	    0,   1,   0,   1,     0,     0,     1,     0,     0,  #total=c1sum+c12+c22
#	    1,   0,   1,   0,     0,     0,     0,     1,     0   #total=c2sum+c11+c21
#		)
#
#	X<-matrix(X,ncol=9, byrow=TRUE)
#	my.frame <- as.data.frame(X)
#	colnames(my.frame) <- c("c11", "c12", "c21", "c22", "r1sum", "r2sum", "c1sum", "c2sum", "total" )
#	
## add the responses y
#	my.frame <- cbind(y, my.frame)
#	
#	res <- lm(y~ c11 + c12 + c21 + c22 + r1sum + r2sum + 
#			  c1sum + c2sum + total + (-1) ,data=my.frame)
#	
#	return(res)
#}


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

    if (se<=0) {
        comment <- paste("se<=0", comment, sep=", ")
        succeeded <- FALSE
    }

    return(list(succeeded=succeeded, comment=comment))

}


########################################################################################
########################################################################################
########################################################################################
########################################################################################

fillin.cont.1spell <- function(n=NA, mean=NA, sd=NA, se=NA, var=NA, 
                         low=NA, high=NA, pval=NA, alpha=0.05) {

    succeeded <- FALSE 
    comment <- ""
    res <- list(succeeded= succeeded)

    z <- abs(qnorm(alpha/2))

    input.vector <- c(n, mean, sd, se, var, low, high, pval)
    input.pattern <- !(is.na(input.vector))

    ##########################################################
    # check the mean first 
    # If not calculate it from the CI

    if(is.na(mean)) {
        mean = try((high+low)/2, silent = TRUE)
    }

    # if mean is still missing, abort 
    if(is.na(mean)) {
        comment <- paste(comment, "no info on mean", sep="|")
        return(c(res, comment=comment))
    }


    ##########################################################
    # if se is missing
    # try the variance 

    if(is.na(se)) {
        se=try(sqrt(var), silent=TRUE)
    }

    # try the sd and the n
    if(is.na(se)) {
        se=try(sqrt(sd^2)/(n-1), silent=TRUE)
    }

    # try both ends of the CI
    if(is.na(se)) {
        se=try(abs(high-low)/(2*z), silent=TRUE)
    }

    # try low end of CI
    if(is.na(se)) {
        se=try((mean-low)/z, silent=TRUE)
    }

    # try high end of CI
    if(is.na(se)) {
        se=try((high-mean)/z, silent=TRUE)
    }

    # try the 2 sided p-value 
    if(is.na(se)) {
        se=try( -mean/qnorm(pval/2) , silent=TRUE)
    }
   
    # if the se is still missing, then abort 
    if(is.na(se)) {
       comment <- paste(comment, "no info on dispersion", sep="|")
        return(c(res, comment=comment))
    }


    ##########################################################
    # if the variance is missing 
    if(is.na(var)) {
        var = se^2 
    }
    
    ##########################################################
    # if the lower CI is missing 
    if(is.na(low)) {
        low = mean - z* se 
    }
    
    ##########################################################
    # if the high CI is missing 
    if(is.na(high)) {
        high = mean + z* se 
    }

    ##########################################################
    # if the 2 sided pval is missing 
    if(is.na(pval)) {
        pval = 2*pnorm(-abs(mean/se))
    }

    ##########################################################
    # if the sd is missing 
    if(is.na(sd)) {
        sd=try( var*(n-1), silent=TRUE)
    }

    if(is.na(sd)) {
        comment <- paste(comment, "{n & sd} missing")
    }

    ##########################################################
    # if the n is missing 
    if(is.na(n)) {
        n=try( round( sd/var +1  ), silent=TRUE)
    }

    succeeded <- check.1spell.res(n=n, se=se)$succeeded



    output.vector <- c(n, mean, sd, se, var, low, high, pval)
    output.names <- c("n", "mean", "sd", "se", "var", "low", "high", "pval")
    names(output.vector) <- output.names

    res<- list(succeeded=succeeded, input.pattern=input.pattern, output=output.vector, comment=comment)
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