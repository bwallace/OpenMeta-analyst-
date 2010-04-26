isnt.null <- function(x){
    # some syntactic sugar.
    ! is.null(x)    
}


##############
#
# Binary data calculation
#
##############


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


# see end for an example code


find.my.cells <- function(cell.counts, logor, var, 
					row1sum, row2sum, col1sum, col2sum) {
	
	# first equation: c11 + c12 = N1*
	eq1 <- row.total.eq(cell.counts, row=1, total=row1sum)
	
	# second equation: c21 + c22 = N2*
	eq2 <- row.total.eq(cell.counts, row=2, total=row2sum)

	# third equation: c11 + c21 = N*1
	eq3 <- col.total.eq(cell.counts, col=1, total=col1sum)
	
	# fourth equation: c12 + c22 = N*2
	eq4 <- col.total.eq(cell.counts, col=2, total=col2sum)
	
	# fifth equation: log(c11*c22)-log(c12*c21) = logor
	eq5 <- logor.eq(cell.counts, logor= logor)
	
	# sixth equation: 1/c11 + 1/c12 + 1/c21 + 1/c22 = var 
	eq6 <- var.eq(cell.counts, var = var)
	
	res <- eq1 + eq2 + eq3 + eq4 + eq5 + eq6
	return(1000*res)
}



row.total.eq <- function(cell.counts, row, total=NA) {
	res <- cell.counts[(row-1)*2+1] + cell.counts[(row-1)*2+2]
	if (is.na(total)) {
		return(res)
	}
	else {
		return((res-total)^2)
	}
	
} 


col.total.eq <- function(cell.counts, col, total=NA) {
	res <- cell.counts[col] + cell.counts[2+col]
	if (is.na(total)) {
		return(res)
	}
	else {
		return((res-total)^2)
	}
	
} 


logor.eq <- function(cell.counts, logor=NA, cc=0.5) {
	if (cell.counts[1]*cell.counts[2]*cell.counts[3]*cell.counts[4] !=0) {
		res <- log(cell.counts[1]*cell.counts[4]) -
		       log(cell.counts[2]*cell.counts[3])
	}
	else {
		res <- log(cell.counts[1]+cc) +  
		       log(cell.counts[4]+cc) -
		       log(cell.counts[2]+cc) -
		       log(cell.counts[3]+cc)
	}
	
	if (is.na(logor)) {
		return(res)
	}
	else {
		return((res - logor)^2)
	}
}
	
var.eq <- function(cell.counts, var=NA, cc=0.5) {
	if (cell.counts[1]*cell.counts[2]*cell.counts[3]*cell.counts[4] >0) {
		res <- 1/cell.counts[1] +
		       1/cell.counts[2] +
		       1/cell.counts[3] +
		       1/cell.counts[4]
	}
	else {
		res <- 1/(cell.counts[1]+cc) +
			   1/(cell.counts[2]+cc) +
			   1/(cell.counts[3]+cc) + 
			   1/(cell.counts[4]+cc)
	}
	if (is.na(var)) {
		return(res)		
	}
	else {
		return((res - var)^2)
	}
}


#var.sq.gradients <- function(cell.counts, var = NA) {
#	# returns partial first derivatives of squared variances 
#
#	d1 <- -2 * var.eq(cell.counts, var=var) / cell.counts[1]^2
#	d2 <- -2 * var.eq(cell.counts, var=var) / cell.counts[2]^2
#	d3 <- -2 * var.eq(cell.counts, var=var) / cell.counts[3]^2
#	d4 <- -2 * var.eq(cell.counts, var=var) / cell.counts[4]^2
#	res <- c(d1,d2,d3,d4)
#	
#	return(res)
#}

#logor.sq.gradients <- function(cell.counts, logor = NA) {
#	# returns partial first derivatives of squared logor
#
#	d1 <-  2 * logor.eq(cell.counts, logor=logor) / cell.counts[1]
#	d2 <- -2 * logor.eq(cell.counts, logor=logor) / cell.counts[2]
#	d3 <- -2 * logor.eq(cell.counts, logor=logor) / cell.counts[3]
#	d4 <-  2 * logor.eq(cell.counts, logor=logor) / cell.counts[4]
#	
#	res <- c(d1,d2,d3,d4)
#	
#	return(res)
#}



#gradients<- function(cell.counts, logor, var, 
#					row1sum, row2sum) {
#	# returns the gradients for the cell counts 
#	# note that I have to create them from the 
#	# four components of the 
#	# objective function. 	
#	
#	row1.sum.part <- 2*c(rep((sum(cell.counts[1:2]) - row1sum),2), 0, 0)
#	row2.sum.part <- 2*c(0 , 0, rep((sum(cell.counts[3:4]) - row2sum),2)) 
#	variance.part <- var.sq.gradients(cell.counts, var = var)
#	logor.part <- logor.sq.gradients(cell.counts, logor = logor)
#	
#	res <- row1.sum.part + row2.sum.part 
#		   variance.part + logor.part 
#	
#	return(res)
#}


############################################

# make a fictitious 2x2
a <- c(1001, 20, 300, 40)
# calculate the logor and the variance 
my.logor <- logor.eq(a, logor=NA)
my.var <- var.eq(a, var=NA)
my.row1sum <- sum(a[c(1,2)])
my.row2sum <- sum(a[c(3,4)])
my.col1sum <- sum(a[c(1,3)])
my.col2sum <- sum(a[c(2,4)])
my.total <- sum(a)

result<-optim(rep(my.total/4,4), find.my.cells, gr=NULL, logor=my.logor,var=my.var,
	row1sum=my.row1sum, row2sum =my.row2sum, col1sum=my.col1sum, col2sum=my.col2sum,
	method = "Nelder-Mead",
	control=c(abstol=1e-8, maxit=10000, reltol=0))

result

round(result$par)






############################
#
# Continuous data calculation
# --- 
# The following code is due to Tom Trikalinos.
# Originally in fillin.continuous.r file.
#
############################


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
        mean = try((high-low)/2, silent = TRUE)
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
########################################################################################
########################################################################################
########################################################################################
fillin.cont.AminusB <- function(
    n.A=NA, mean.A=NA, sd.A=NA, se.A=NA, var.A=NA, low.A=NA, high.A=NA, pval.A=NA, 
    n.B=NA, mean.B=NA, sd.B=NA, se.B=NA, var.B=NA, low.B=NA, high.B=NA, pval.B=NA,
    correlation = 0, alpha=0.05) {


    succeeded <- TRUE  
    comment <- ""
    res <- list(succeeded= succeeded)

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
        n.diff  <- try(fillin.A$output["n"] + fillin.B$output["n"], silent=TRUE)
        sd.diff <- try(var.diff * (n.diff - 1) , silent=TRUE)
    } 
    else {
        succeeded <- FALSE 
    }


    output.vector <- c(n.diff, mean.diff, sd.diff, se.diff, var.diff, 
                      low.diff, high.diff, pval.diff)
    output.names <- c("n", "mean", "sd", "se", "var", "low", "high", "pval")
    names(output.vector) <- output.names

    res<- list(succeeded=succeeded, input.pattern=input.pattern, output=output.vector, 
                      comment=comment, correlation=correlation)

    return(res)

}






