isnt.null <- function(x){
    # some syntactic sugar.
    ! is.null(x)    
}


############################ 
# Binary data calculation  #
############################
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


fillin.2x2.simple <- function(c11=NA, c12=NA, c21=NA, c22=NA, 
                                                r1sum =NA, r2sum=NA, 
                                                c1sum=NA, c2sum=NA,
                                                total=NA, touse=rep(TRUE,9)){
	y <- c(
	rep(c11,4), 
	rep(c12,4),
	rep(c21,4), 
	res<-rep(c22,4),
	rep(r1sum,3), 
	rep(r2sum,3), 
	rep(c1sum,3), 
	rep(c2sum,3),
	rep(total,8)) 

    select <- c(
		rep(touse[1],4), 
		rep(touse[2],4), 
		rep(touse[3],4), 
		rep(touse[4],4), 
		rep(touse[5],3), 
		rep(touse[6],3), 
		rep(touse[7],3), 
		rep(touse[8],3), 
		rep(touse[9],8))

	X<- c(
	# c11, c12, c21, c22, r1sum, r2sum, c1sum, c2sum, total 
	    1,   0,   0,   0,     0,     0,     0,     0,     0,  #c11
	    0,  -1,   0,   0,     1,     0,     0,     0,     0,  #c11=r1sum-c12
	    0,   0,  -1,   0,     0,     0,     1,     0,     0,  #c11=c1sum-c21
	    0,  -1,  -1,  -1,     0,     0,     0,     0,     1,  #c11=total-c12-c21-c22

	    0,   1,   0,   0,     0,     0,     0,     0,     0,  #c12
	   -1,   0,   0,   0,     1,     0,     0,     0,     0,  #c12=r1sum-c11
	    0,   0,   0,  -1,     0,     0,     0,     1,     0,  #c12=c2sum-c22
	   -1,   0,  -1,  -1,     0,     0,     0,     0,     1,  #c12=total-c11-c21-c22

	    0,   0,   1,   0,     0,     0,     0,     0,     0,  #c21
	    0,   0,   0,  -1,     0,     1,     0,     0,     0,  #c21=r2sum-c22
	   -1,   0,   0,   0,     0,     0,     1,     0,     0,  #c21=c1sum-c11
	   -1,  -1,   0,  -1,     0,     0,     0,     0,     1,  #c21=total-c11-c12-c22

	    0,   0,   0,   1,     0,     0,     0,     0,     0,  #c22
	    0,   0,  -1,   0,     0,     1,     0,     0,     0,  #c22=r2sum-c21
	    0,  -1,   0,   0,     0,     0,     0,     1,     0,  #c22=c2sum-c12
	   -1,  -1,  -1,   0,     0,     0,     0,     0,     1,  #c22=total-c11-c12-c21
	
	    0,   0,   0,   0,     1,     0,     0,     0,     0,  #r1sum
	    1,   1,   0,   0,     0,     0,     0,     0,     0,  #r1sum=c11+c12
	    0,   0,   0,   0,     0,    -1,     0,     0,     1,  #r1sum=total-r2sum

	    0,   0,   0,   0,     0,     1,     0,     0,     0,  #r2sum
	    0,   0,   1,   1,     0,     0,     0,     0,     0,  #r2sum=c21+c22
	    0,   0,   0,   0,    -1,     0,     0,     0,     1,  #r2sum=total-r1sum
	    
	    0,   0,   0,   0,     0,     0,     1,     0,     0,  #c1sum
	    1,   0,   1,   0,     0,     0,     0,     0,     0,  #c1sum=c11+c21
	    0,   0,   0,   0,     0,     0,     0,    -1,     1,  #c1sum=total-c2sum

	    0,   0,   0,   0,     0,     0,     0,     1,     0,  #c2sum
	    0,   1,   0,   1,     0,     0,     0,     0,     0,  #c2sum=c12+c22
	    0,   0,   0,   0,     0,     0,    -1,     0,     1,  #c2sum=total-c1sum

	    0,   0,   0,   0,     0,     0,     0,     0,     1,  #total
	    1,   1,   1,   1,     0,     0,     0,     0,     0,  #total=c11+c12+c21+c22
	    0,   0,   0,   0,     1,     1,     0,     0,     0,  #total=r1sum+r2sum
	    0,   0,   0,   0,     0,     0,     1,     1,     0,  #total=c1sum+c2sum
	    0,   0,   1,   1,     1,     0,     0,     0,     0,  #total=r1sum+c21+c22
	    1,   1,   0,   0,     0,     1,     0,     0,     0,  #total=r2sum+c11+c12
	    0,   1,   0,   1,     0,     0,     1,     0,     0,  #total=c1sum+c12+c22
	    1,   0,   1,   0,     0,     0,     0,     1,     0   #total=c2sum+c11+c21
		)

	X<-matrix(X,ncol=9, byrow=TRUE)
	my.frame <- as.data.frame(X)
	colnames(my.frame) <- c("c11", "c12", "c21", "c22", "r1sum", "r2sum", "c1sum", "c2sum", "total" )
	
# add the responses y
	my.frame <- cbind(y, my.frame)
	
	res <- lm(y~ c11 + c12 + c21 + c22 + r1sum + r2sum + 
			  c1sum + c2sum + total + (-1) ,data=my.frame)
	
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
impute.diagnostic.data <- function(diag.data, metric){
    # this function imputes diagnostic data (i.e., 2x2 tables) from the fields
    # available in the diagnostic.data data frame parameter.
    #
    if (metric=="Sens") {
      ci.data <- list(estimate=diag.data$sens, lb=diag.data$sens.lb, ub=diag.data$sens.ub, conf.level=diag.data$conf.level)
      est.var <- calc.est.var(ci.data)
      # fill in estimate, ci.lb, ci.ub
      diag.data$sens <- est.var$estimate
      diag.data$sens.var <- est.var$var
      diag.data <- calc.sens.data(diag.data)
    } else if (metric=="Spec") {
      ci.data <- list(estimate=diag.data$spec, lb=diag.data$spec.lb, ub=diag.data$spec.ub, conf.level=diag.data$conf.level)
      est.var <- calc.est.var(ci.data)
      # fill in estimate, ci.lb, ci.ub
      diag.data$spec <- est.var$estimate
      diag.data$spec.var <- est.var$var
      diag.data <- calc.spec.data(diag.data)
    }  
    diag.data 
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

calc.sens.data <- function(diag.data) {
  # back-calculate TP, FN, sens, var from any two known values
  # Notes:  TP = 1 / (( 1-sens) * var)
  #         FN = 1 / (sens * var)
  #         TP / FN = sens / (1-sens)
  #         FN / TP = (1-sens) / sens
  #
  TP<-NULL; FN<-NULL; TN<-NULL; FP<-NULL; sens<-NULL; sens.var<-NULL
  
    
  if (isnt.null(diag.data$sens) & isnt.null(diag.data$sens.var)) {
    sens <- diag.data$sens
    sens.var <- diag.data$sens.var
    diag.data$TP <- round(1 / ((1-sens) * sens.var), digits=0)
    diag.data$FN <- round(1 / (sens * sens.var), digits=0)
  } else if (isnt.null(diag.data$sens) & isnt.null(diag.data$TP)) {
    sens <- diag.data$sens 
    TP <- diag.data$TP
    diag.data$FN <- round(((1 - sens) / sens) * TP, digits=0)
    diag.data$sens.var <- 1 / ((1 - sens) * TP)
  } else if (isnt.null(diag.data$sens) & isnt.null(diag.data$FN)) {
    sens <- diag.data$sens 
    FN <- diag.data$FN
    diag.data$TP <- round((sens / (1 - sens)) * FN, digits=0)
    diag.data$sens.var <- 1 / (sens * FN)
  } else if (isnt.null(diag.data$sens.var) & isnt.null(diag.data$TP)) {
    sens.var <- diag.data$sens.var 
    TP <- diag.data$TP
    sens <- 1 - 1/(TP * sens.var)
    diag.data$sens <- sens
    diag.data$FN <- round(1 / (sens * sens.var), digits=0)
  } else if (isnt.null(diag.data$sens.var) & isnt.null(diag.data$FN)) {
    sens.var <- diag.data$sens.var 
    FN <- diag.data$FN
    sens <- 1 / (FN * sens.var)
    diag.data$sens <- sens
    diag.data$TP <- round(1 / ((1-sens) * sens.var), digits=0)
  }     
  diag.data
}

calc.spec.data <- function(diag.data) {
  # back-calculate TN, FP, spec, var from any two known values
  # Note: This function is identical to calc.sens.data with the following substitutions:
  # sens -> spec
  # TP -> TN
  # FN -> FP
  # Could combine into one function, but it would be less readable. 
  #
  # Notes:  TN = 1 / (( 1-spec) * var)
  #         FP = 1 / (spec * var)
  #         TN / FP = spec / (1-spec)
  #         FP / TN = (1-spec) / spec
  #
  TN<-NULL; FP<-NULL; TN<-NULL; FP<-NULL; spec<-NULL; spec.var<-NULL
  
    
  if (isnt.null(diag.data$spec) & isnt.null(diag.data$spec.var)) {
    spec <- diag.data$spec
    spec.var <- diag.data$spec.var
    diag.data$TN <- round(1 / ((1-spec) * spec.var), digits=0)
    diag.data$FP <- round(1 / (spec * spec.var), digits=0)
  } else if (isnt.null(diag.data$spec) & isnt.null(diag.data$TN)) {
    spec <- diag.data$spec 
    TN <- diag.data$TN
    diag.data$FP <- round(((1 - spec) / spec) * TN, digits=0)
    diag.data$spec.var <- 1 / ((1 - spec) * TN)
  } else if (isnt.null(diag.data$spec) & isnt.null(diag.data$FP)) {
    spec <- diag.data$spec 
    FP <- diag.data$FP
    diag.data$TN <- round((spec / (1 - spec)) * FP, digits=0)
    diag.data$spec.var <- 1 / (spec * FP)
  } else if (isnt.null(diag.data$spec.var) & isnt.null(diag.data$TN)) {
    spec.var <- diag.data$spec.var 
    TN <- diag.data$TN
    spec <- 1 - 1/(TN * spec.var)
    diag.data$spec <- spec
    diag.data$FP <- round(1 / (spec * spec.var), digits=0)
  } else if (isnt.null(diag.data$spec.var) & isnt.null(diag.data$FP)) {
    spec.var <- diag.data$spec.var 
    FP <- diag.data$FP
    spec <- 1 / (FP * spec.var)
    diag.data$spec <- spec
    diag.data$TN <- round(1 / ((1-spec) * spec.var), digits=0)
  }          
  diag.data
}


