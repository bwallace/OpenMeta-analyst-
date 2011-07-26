library(BRugs) 
     
run.multi <-
function (Data, outcomes, miss.patterns, N, col.study = "TRIAL", col.treat = "TREAT", covs = NULL, model = "baseline", var.model = "ind",
    nChains = 3, conv.limit = 1.1, niters = 300000, nruns = 5000, setsize = 2000, dprior = list("norm",0,1e-6),slopeprior = list("norm",0,1e-6), 
    Etaprior=list("norm",0,0.25), varprior=list("prec","wish"),varprior.params = c(1,5), path = "C:/dev/OpenMeta-analyst-/R/multinomial_tmp/multinomial/")
{
# Data is name of R data frame with rows and all columns including incomplete ones
# outcomes is set of columns with outcome data including summed columns
# miss.patterns is list with missing data patterns
#    each element of list is separate missing data pattern with three subelements
#    first subelement is set of rows of dat corresponding to this missing data pattern
#    second subelement is indicator matrix where each row gives linear combination of columns of outcome data producing one multinomial element
#    third subelement is indicator matrix where each rows gives linear combination of full set of multinomial probabilities corresponding to collapsed set of probabilities
# col.study is column with trial labels
# col.treat is column with treatment labels
# covs is column number(s) of covariates in meta-regression model; if none given, it is NULL
# model is either "baseline" for baseline logit models or "constant" for model that compares each category to all others
# var.model is "homogeneous","independence", or "consistent" (see Lu and Ades, 2009)
# nChains is number of MCMC chains to run
# conv.limit is limit for BGR convergence
# niters is number of MCMC iterations to run
# nruns is number of MCMC iterations to save
# setsize is number of MCMC iterations after which convergence diagnostics should be checked
# dprior is prior for treatment effects as 3-list (distribution, mean, precision)
# slopeprior is prior for slope as 3-list (distribution, mean, precision)
# Etaprior is prior for random study effects as 3-list (distribution, mean, precision)
# varprior is 2-list of parameter and distribution upon which prior for variance is put 
#    choices for parameter: precision, variance, sd
#    choices for distribution: gamma, Wishart, uniform
# varprior.params is set of parameters of variance distribution
# path is directory where data are stored
#
        if (missing(miss.patterns)) {
			arms <- seq(dim(Data)[1])
#			 miss.patterns = NULL
			ncat = length(outcomes)
			miss.patterns = list(list(list(seq(arms),seq(ncat))) ,diag(ncat))
        }
	    else {
            arms <- vector("integer")
			ncat <- dim(miss.patterns[[2]])[2]  # Number of outcome categories
            for (i in seq(length(miss.patterns[[1]])))
                arms <- c(arms,miss.patterns[[1]][[i]][[1]])
        }
        narm <- length(arms)
        dat <- Data[arms,]     # Reorder data in order of missing data patterns
        N <- N[arms]
		study <- sort.vec(dat[, col.study])
		treat <- sort.vec(dat[, col.treat])
        nstudy <- length(unique(study))
        ntreat <- length(unique(treat))
        ref <- 1  #Reference treatment
        base <- make.base.treat(dat,col.study,col.treat,narm)
        b.id <- base$b.id
        base.treat <- base$base.treat
        D <- Data[, outcomes]	#Outcomes for studies
		arms.by.study <- rle(dat[,col.study])$lengths #Number of arms in studies
        nitems <- dim(D)[1]  # number of complete data rows
        if(is.null(covs)) {
            x <- NULL
            pars.to.save <- c("d", "Eta", "Delta", "Prec", "P", "dev")
        }
        else {
            x <- as.matrix(dat[seq(nitems), covs])
            pars.to.save <- c("d", "slope", "Eta", "Delta", "Prec", "P", "dev")
        }
		prior <- multi.prior(ncat,covs,dprior,Etaprior,slopeprior,varprior,varprior.params)
        multi.inits(D, x, treat, study, nChains, narm, ntreat, ncat, nstudy, arms.by.study, b.id, model,var.model, miss.patterns,N, path)
        multi.data(Data, covs, outcomes, miss.patterns, N, prior, narm, ntreat, ncat, nstudy, study, treat, base.treat, path)
		multi.model(dat,miss.patterns,narm, ncat, model, prior,covs, var.model, varprior, path)
        bugs.out <- multi.bugs.fit("model.txt", "data.txt", nChains, paste("inits", seq(nChains), ".txt", sep = ""), path, pars.to.save, setsize, conv.limit, niters, nruns, covs)
        burn.in <- bugs.out[[1]]
        no.runs <- bugs.out[[2]]
        stats <- bugs.out[[3]]
#		dic <- bugs.out[[4]]
        samples <- bugs.out[[4]]
        varnames <- dimnames(samples)[[3]]
        nvars <- dim(samples)[3]
        d.vars <- grep("d", varnames)
        d <- aperm(array(matrix(samples[, , d.vars], c(no.runs * nChains, length(d.vars))), c(no.runs * nChains, ncat - 1, ntreat - 1)), c(1, 3, 2))
        P.vars <- grep("P", varnames)
        P <- aperm(array(matrix(samples[, , P.vars], c(no.runs * nChains, length(P.vars))), c(no.runs * nChains, ncat, narm)), c(1, 3, 2))
        Prec.vars <- grep("Prec", varnames)
        Prec <- aperm(array(matrix(samples[, , Prec.vars], c(no.runs * nChains, length(Prec.vars))), 
                            c(no.runs * nChains, ncat - 1, ncat - 1, ntreat - 1, ntreat - 1)), c(1, 5, 4, 3, 2))
#		dev.vars = grep("dev", varnames)
#		dev <- aperm(array(matrix(samples[, , dev.vars], c(no.runs * nChains, length(dev.vars))), c(no.runs * nChains, ncat, narm)), c(1, 3, 2))
        if (!is.null(covs)) {
        	slope.vars <- grep("slope", varnames)
        	slope <- aperm(array(matrix(samples[, , slope.vars], c(no.runs * nChains, length(slope.vars))), c(no.runs * nChains, ncat - 1, ntreat - 1)), c(1, 3, 2))
        }
        if (is.null(covs)) {
#			out <- list(burn.in, no.runs, stats, dic, d, Prec, P, samples)
#			names(out) <- c("Burn In", "Number runs per chain", "Stats","DIC Stats","d","Prec","P", "MCMC Samples")
			out <- list(burn.in, no.runs, stats, d, Prec, P, D, N, ncat, miss.patterns, samples)
			names(out) <- c("Burn In", "Number runs per chain", "Stats","d","Prec","P", "Data", "N", "No.Categories", "Missing.Data.Patterns", "MCMC Samples")
		}
		else {
			out <- list(burn.in, no.runs, stats, d, slope, Prec, P, D, N, ncat, miss.patterns, samples)
			names(out) <- c("Burn In", "Number runs per chain", "Stats","d","Slopes","Prec","P", "Data", "N", "No.Categories", "Missing.Data.Patterns", "MCMC Samples")
		}
        return(out)
}

make.base.treat <-
function (dat,col.study,col.treat,narm)
{
# Create base treatment vector and base treat id vector
        study.lengths <- rle(dat[,col.study])$lengths
        nstudy <- length(study.lengths)
        ends <- cumsum(study.lengths)                                # End row of trials
        starts <- c(1, ends[-length(ends)] + 1)            # Start row of trials
        b.treat <- rep(NA, nstudy)
        b.id <- rep(F, narm)
        for (i in 1:nstudy) {
                limits <- starts[i]:ends[i]                      # Start and end rows of study i
                b.treat[i] <- min(dat[limits, col.treat])                # Base treatment (lowest numbered) for study i
                b.id[limits[b.treat[i] == dat[limits, col.treat]]] <- T  # True if arm is base treatment
        }
        base.treat <- rep(b.treat, each = study.lengths)             # Vector of base treat by study replicated within study
        out <- list(b.id,base.treat)
        names(out) <- c("b.id","base.treat")
        return(out)
}


multi.prior<-
function (ncat,covs,dprior,Etaprior,slopeprior,varprior,varprior.params) 
{
	if (ncat > 2) {
		mean.d = rep(dprior[[2]],ncat-1)
		Prec.d = diag(dprior[[3]],ncat-1,ncat-1)
		Prior.d <- paste("dm",dprior[[1]],"(mean.d[], Prec.d[,])",sep="")
		mean.Eta = rep(Etaprior[[2]],ncat-1)
		Prec.Eta = diag(Etaprior[[3]],ncat-1,ncat-1)
		Prior.Eta <- paste("dm",Etaprior[[1]],"(mean.Eta[],Prec.Eta[,])",sep="")
		if (!is.null(covs)) {
			mean.slope = rep(slopeprior[[2]],ncat-1)
			Prec.slope = dag(slopeprior[[3]],ncat-1,ncat-1)
			Prior.slope <- paste("dm",slopeprior[[1]],"(mean.slope[],Prec.slope[,])",sep="")
		}
		
		if (varprior[[1]] == "prec") {
			if (varprior[[2]] == "wish") {
				Prec.1 = diag(varprior.params[1],ncat-1)
				Prec.2 = varprior.params[2]
				Prior.Prec <- paste("d",varprior[[2]],"(Prec.1[,], Prec.2)",sep="")
			}
			else return("Only Wishart currently supported for multinomial precision distribution")
		}
	}
	else {
		mean.d = dprior[[2]]
		Prec.d = dprior[[3]]
		Prior.d <- paste("d",dprior[[1]],"(mean.d,Prec.d)",sep="")
		mean.Eta = Etaprior[[2]]
		Prec.Eta = Etaprior[[3]]
		Prior.Eta <- paste("d",Etaprior[[1]],"(mean.Eta,Prec.Eta)",sep="")
		if (!is.null(covs)) {
			mean.slope = slopeprior[[2]]
			Prec.slope = slopeprior[[3]]
			Prior.slope <- paste("d",slopeprior[[1]],"(mean.slope,Prec.slope)",sep="")
		}
		
		if (varprior[[1]] == "prec") {
			if (varprior[[2]] == "gamma") {
				Prec.1 = varprior.params[1]
				Prec.2 = varprior.params[2]
				Prior.Prec <- paste("d",varprior[[2]],"(Prec.1, Prec.2)",sep="")
			}
			else return("Only Gamma currently supported for precision distribution")
		}
		else if (varprior[[1]] == "sd") {
			if (varprior[[2]] == "unif") {
				Prec.1 = varprior.params[1]
				Prec.2 = varprior.params[2]
				Prior.Prec <- paste("d",varprior[[2]],"(Prec.1, Prec.2)",sep="")
			}
			else return("Only Uniform currently supported for sd distribution")
		}
	}
#	  Prec.1 <- 1
#	  Prec.d <- 0.000001
#	  Prec.slope <- 0.000001
#	  Prec.2 <- 1
#	  Prec.Eta <- .25
#	}

   if (is.null(covs)) {
	  out <- list(mean.d, mean.Eta, Prec.1, Prec.2, Prec.d, Prec.Eta, Prior.Prec, Prior.d, Prior.Eta)
	  names(out) <- c("mean.d", "mean.Eta", "Prec.1", "Prec.2", "Prec.d", "Prec.Eta", "Prior.Prec", "Prior.d", "Prior.Eta")
   }
   else {
	   out <- list(mean.slope, mean.d, mean.Eta, Prec.1, Prec.2, Prec.d, Prec.Eta, Prec.slope,
				   Prior.Prec, Prior.d, Prior.Eta, Prior.slope)
	   names(out) <- c("mean.slope", "mean.d", "mean.Eta", "Prec.1", "Prec.2", "Prec.d", "Prec.Eta", "Prec.slope", 
					  "Prior.Prec", "Prior.d", "Prior.Eta", "Prior.slope")
   }
   return(out)
}

multi.inits <-
function (D, x, treat, study, nChains, narm, ntreat, ncat, nstudy, arms.by.study, b.id, model,var.model, miss.patterns, N, path) 
{
# Computes initial values for baseline category logits model without covariates
# Inputs:
#        D                   matrix of data outcomes
#        x                   matrix of predictors
#        treat               vector of treatment labels
#        study               vector of study labels
#        nChains             number of MCMC chains
#        narm                number of study arms
#        ntreat              number of treatments
#        ncat                number of outcome categories
#        nstudy              number of studies
#        arms.by.study       number of arms in each study
#        b.id                indicator for base treatment in arms
#        model               "baseline" if baseline category logits model (alternative is logit model)
#        var.model           model for variance ("ind","hom","cons")
#        miss.patterns       missing data patterns
#        N                   number observations by study
#        path                path to save initial value file
	if (length(miss.patterns[[1]])==1)
		Dimputed = D
	else
		Dimputed = multi.impute.data(D,miss.patterns,ncat,N)
    
	if (model == "baseline") {
		logits <- as.matrix(log(Dimputed[, -1]) - log(Dimputed[, 1]))
		se.logits <- as.matrix(sqrt(1/Dimputed[, -1] + 1/Dimputed[, 1]))
		Eta <- se.Eta <- matrix(NA, nstudy, ncat)
		Eta[, 2:ncat] <- logits[b.id, ]
		se.Eta[, 2:ncat] <- se.logits[b.id, ]
		delta <- logits - apply(as.matrix(Eta[, -1]), 2, rep, times = arms.by.study)
	}
	else if (model == "logit") {
		N <- apply(Dimputed, 1, sum)
		logits <- as.matrix(log(Dimputed/(N-Dimputed)))
		se.logits <- as.matrix(sqrt(1/Dimputed + 1/(N-Dimputed)))
		Eta <- se.Eta <- matrix(NA, nstudy, ncat-1)
		Eta <- as.matrix(logits[b.id,-1])
		se.Eta <- as.matrix(se.logits[b.id,-1])
		delta <- logits[,-1] - apply(Eta,2,rep,times=arms.by.study)
	}
	d <- se.d <- matrix(NA, length(unique(treat)), ncat-1)
	rows.of.basetreat <- seq(dim(Dimputed)[1])*as.numeric(b.id)
	delta <- delta[-rows.of.basetreat,,drop=F]   # Eliminate base treatment arms
	base.tx <- treat[b.id]    # base treatment for N studies
	end.study <- c(0, cumsum(arms.by.study))  # end row number of each trial
	rows <- end.study - seq(0, nstudy)   # end number of each trial not including base treatment arms
	design.mat <- matrix(0, narm-nstudy, ntreat)
	for (i in seq(nstudy)) {
		studytx <- treat[(end.study[i]+1):end.study[i+1]]  #treatments in ith study
		nonbase.tx <- studytx[studytx!=base.tx[i]]    #non-baseline treatments for ith study
		design.mat[(1+rows[i]):rows[i+1],base.tx[i]] <- -1
		for (j in seq(length(nonbase.tx)))
			design.mat[j+rows[i],nonbase.tx[j]] <- 1	
	}
	design.mat <- design.mat[,-1,drop=F]
	unique.treat = sort(unique(treat))
	if (!is.null(x)) {
		ncov <- dim(x)[2]
		slope <- se.slope <- array(NA, c(length(unique(treat)), ncov, ncat-1))
	}
	for (k in 1:(ncat-1)) {
		if (is.null(x)) {
			fit <- summary(lm(delta[, k] ~ design.mat - 1))
		}
		else {
			x.cen <- x[-rows.of.basetreat,,drop=F]
			x.cen <- x.cen - apply(x.cen,2,mean)
			fit <- summary(lm(delta[,k]~ design.mat + design.mat:x.cen-1))
			slope[treat[-1],,k] <- coef(fit)[ntreat-1 + seq((ntreat-1)*ncov),1]
			if (!is.nan(fit$fstat[1])) se.slope[treat[-1],,k] <- coef(fit)[ntreat-1 + seq((ntreat-1)*ncov),2]
			else se.slope[treat[-1],,k] <- 1
		}
		d[unique.treat[-1],k] <- coef(fit)[1:(ntreat-1), 1]
		if (!is.nan(fit$fstat[1])) {
			se.d[unique.treat[-1],k] <- coef(fit)[1:(ntreat-1), 2]
			tau <- fit$sigma^2
		}
		else {
			se.d[unique.treat[-1],k] <- 1
			tau = 1
		}
		d[1,k] <- se.d[1,k] <- NA
		if (!is.null(x)) 
			slope[1,,k] <- se.slope[1,,k] <- NA
	}
	if (model == "baseline") {
		Delta <- matrix(NA, narm, ncat)
		Delta[b.id,2:ncat] <- 0
		Delta[seq(narm)[!b.id],2:ncat] <- delta
	}
	else if (model == "logit") {
		Delta <- matrix(NA, narm, ncat-1)
		Delta[b.id,] <- 0
		Delta[seq(narm)[!b.id],] <- delta
	}
	random.Eta1 <- matrix(rnorm(dim(Eta)[1]*dim(Eta)[2]),dim(Eta)[1],dim(Eta)[2])
	random.Eta2 <- matrix(rnorm(dim(Eta)[1]*dim(Eta)[2]),dim(Eta)[1],dim(Eta)[2])
	random.d1 <- matrix(rnorm(dim(d)[1]*dim(d)[2]),dim(d)[1],dim(d)[2])
	random.d2 <- matrix(rnorm(dim(d)[1]*dim(d)[2]),dim(d)[1],dim(d)[2])
	if (!is.null(x)) {
		random.slope1 <- matrix(rnorm(dim(slope)[1]*dim(slope)[2]),dim(slope)[1],dim(slope)[2])
		random.slope2 <- matrix(rnorm(dim(slope)[1]*dim(slope)[2]),dim(slope)[1],dim(slope)[2])
	}
	dimD1 <- dim(Dimputed)[1]
	if (var.model == "ind") {
		Prec <- multi.inits.ind(ncat,ntreat,tau)
		if (ncat > 2) {
			random.ISigma1 <- array(rchisq(dim(Prec)[1]*dim(Prec)[2]*dim(Prec)[3]*dim(Prec)[4],dimD1-1),c(dim(Prec)[1],dim(Prec)[2],dim(Prec)[3],dim(Prec)[4]))
			random.ISigma2 <- array(rchisq(dim(Prec)[1]*dim(Prec)[2]*dim(Prec)[3]*dim(Prec)[4],dimD1-1),c(dim(Prec)[1],dim(Prec)[2],dim(Prec)[3],dim(Prec)[4]))
		}
		else {
			random.ISigma1 <- array(rchisq(dim(Prec)[1]*dim(Prec)[2],dimD1-1),c(dim(Prec)[1],dim(Prec)[2]))
			random.ISigma2 <- array(rchisq(dim(Prec)[1]*dim(Prec)[2],dimD1-1),c(dim(Prec)[1],dim(Prec)[2]))
		}
	}
	else if (var.model == "hom") {
		Prec <- multi.inits.hom(ncat,ntreat,tau)
		random.ISigma1 <- rchisq(1,dimD1-1)
		random.ISigma2 <- rchisq(1,dimD1-1)
	}
	else if (var.model == "consis") Prec <- multi.inits.ind(ncat,ntreat,tau)

	
	
	if (is.null(x)) {
		inits.1 <- list(Eta = Eta, d = d, Delta = Delta, Prec = Prec)
#		inits.2 <- list(Eta = Eta + se.Eta/2, d = d, Delta = Delta, Prec = Prec * (dimD1 - 1)/qchisq(0.975, dimD1 - 1))
#		inits.3 <- list(Eta = Eta - se.Eta/2, d = d - se.d/2, Delta = Delta, Prec = Prec * (dimD1 - 1)/qchisq(0.025, dimD1 - 1))
		inits.2 <- list(Eta = Eta + se.Eta*random.Eta1, d = d + se.d*random.d1, Delta = Delta, Prec = Prec * (dimD1 - 1)/random.ISigma1)
		inits.3 <- list(Eta = Eta + se.Eta*random.Eta2, d = d + se.d*random.d2, Delta = Delta, Prec = Prec * (dimD1 - 1)/random.ISigma2)
	}
	else {
		inits.1 <- list(Eta = Eta, d = d, Delta = Delta, slope = slope, Prec = Prec)
#		inits.2 <- list(Eta = Eta + se.Eta/2, d = d, Delta = Delta, slope = slope, Prec = Prec*(dimD1 - 1)/qchisq(0.975, dimD1 - 1))
#		inits.3 <- list(Eta = Eta - se.Eta/2, d = d - se.d/2, Delta = Delta, slope = slope - se.slope/2, Prec = Prec*(dimD1 - 1)/qchisq(0.025, dimD1 - 1))
		inits.2 <- list(Eta = Eta + se.Eta*random.Eta1, d = d + se.d*random.d1, Delta = Delta, slope = slope + se.slope*random.slope1, Prec = Prec*(dimD1 - 1)/random.ISigma1)
		inits.3 <- list(Eta = Eta + se.Eta*random.Eta2, d = d + se.d*random.d2, Delta = Delta, slope = slope + se.slope*random.slope2, Prec = Prec*(dimD1 - 1)/random.ISigma2)
	}
	inInits <- list(inits.1,inits.2,inits.3)
	inits <- c("inits1.txt", "inits2.txt", "inits3.txt")
    bugsInits(inInits, nChains, paste(path, inits, sep = ""))
}


multi.impute.data <- function(D, miss.patterns, ncat, N, probs.to.impute)
{
	D = as.matrix(D)
	Dimputed = matrix(NA,dim(D)[1],ncat)
	count = 0
	if(missing(probs.to.impute)) imputed.prop = rep(1/ncat,ncat)
	else imputed.prop = probs.to.impute
	for (i in seq(length(miss.patterns[[1]]))) {
		rows = miss.patterns[[1]][[i]][[1]]        #data rows in missing data pattern
		cols.data = miss.patterns[[1]][[i]][[2]]   #data columns in missing data pattern
		is.complete.cols = cols.data %in% seq(ncat)   #which data columns are complete
		if (any(is.complete.cols)) {
			complete.cols = cols.data[is.complete.cols] #col no. of complete cols
			incomplete.cols = cols.data[!is.complete.cols] #col nos. of incomplete cols
			Dimputed[rows, complete.cols] = D[rows, complete.cols] #Put in complete data
		}
		else
			incomplete.cols = cols.data
		if (!all(is.complete.cols)) { #If some columns with missing data
			pmat = miss.patterns[[2]][incomplete.cols,,drop=F] #Parameters corresponding to incomplete cols
			if (any(is.complete.cols)) {
				sums.to.split = D[rows, incomplete.cols, drop=F] - D[rows, complete.cols, drop=F]%*%t(pmat[, complete.cols,drop=F])
				pmat[,complete.cols] = 0
				imputed.prop[complete.cols] = 0
			}
			else
				sums.to.split = D[rows, incomplete.cols, drop=F]
			imputed.prop = imputed.prop/sum(imputed.prop)
#			no.to.split = D[rows, incomplete.cols,drop=F] - apply(D[rows,complete.cols,drop=F],1,sum,na.rm=T)
#			no.summands = apply(parsmat,1,sum)  #no. parameters in each linear combination
#			expand.summands = matrix(rep(no.summands,each=length(rows)),ncol=dim(parsmat)[1])
#			no.to.split=round((D[rows, incomplete.cols,drop=F] - apply(D[rows,complete.cols,drop=F],1,sum,na.rm=T)) / expand.summands, 0)#Split combination sums equally
			for (j in seq(length(rows))) {
				x0 = matrix(rep(sums.to.split [j,], each=ncat),ncol=length(incomplete.cols))*t(pmat)
				x1 = imputed.prop*t(pmat)
				x2 = x0*x1/rep(apply(x1,2,sum),each=ncat,ncol=dim(pmat)[1])
#				x1 = matrix(rep(no.to.split[j,], each=ncat), ncol=length(incomplete.cols))
#				x2 = x1*t(pmat) # zero out the unneeded parameters
#				if (any(is.complete.cols)) x2[complete.cols,] = 0 # zero out the rows with complete data
				x2[x2==0] = NA
				x3 = apply(x2, 1, mean, na.rm=T) # average across potential imputed values
				x5 = (N[rows[j]]- sum(Dimputed[rows[j],], na.rm=T))/sum(x3, na.rm=T)  #Factor to adjust imputations
				x6 = round(x3*x5) # Apply factor to imputations
				if (any(is.complete.cols))
					Dimputed[rows[j],seq(ncat)[-complete.cols]] = x6[!is.na(x6)]
				else
					Dimputed[rows[j],seq(ncat)] = x6[!is.na(x6)]
				Dimputed[rows[j],1] = Dimputed[rows[j],1] + N[rows[j]] - sum(Dimputed[rows[j],])  #Correction for rounding so totals add
			}
		}
		if (missing(probs.to.impute)) {
			running.total = apply(Dimputed,2,sum,na.rm=T)
			imputed.prop = running.total/sum(running.total) # Proportion of events in each category
		}
		else
			imputed.prop = probs.to.impute
	}
	return(Dimputed)
}


multi.inits.ind <- function(ncat,ntreat,tau)
{
	if (ncat > 2) 
		Prec <- array(NA, c(ntreat - 1, ntreat, ncat - 1, ncat - 1))
	else 
		Prec <- array(NA, c(ntreat - 1, ntreat))
	for (i in 1:(ntreat - 1)) {
		for (j in (i + 1):ntreat) {
			if (ncat > 2) 
				Prec[i, j, , ] <- (1/tau) * diag(ncat - 1)
			else 
				Prec[i, j] <- 1/tau
		}
	}
	return(Prec)
}

multi.inits.hom <- function(ncat,ntreat,tau)
{
	if (ncat > 2) 
		Prec <- (1/tau) * diag(ncat - 1)
	else 
		Prec <- 1/tau
	return(Prec)
}

multi.data <-
function (dat, covs, outcomes, miss.patterns, N, prior, narm, ntreat, ncat, nstudy, study, treat, base.treat, path) 
{
        if (length(miss.patterns[[1]])==1)
			D = as.matrix(dat[,outcomes])
        else {
			D <- matrix(NA, narm, ncat)
			count <- 0
			for (i in seq(length(miss.patterns[[1]]))) {
				rows <- miss.patterns[[1]][[i]][[1]]    # rows in ith pattern
				ncombs <- length(miss.patterns[[1]][[i]][[2]])    #number of parameter combinations in ith pattern
				D[count + seq(length(rows)), seq(ncombs)] <- as.matrix(dat[rows, outcomes[miss.patterns[[1]][[i]][[2]]]])   # full data for ith pattern
				count <- count + length(rows)     # advance count to next set of rows
           }
        }
        corr.factor <- N/apply(D,1,sum,na.rm=T)    # correction factor for multinomial probabilities to account for duplicate sums
        if (is.null(covs)) {
        	if (length(miss.patterns[[1]])==1)
                inData <- list(narm = narm, ntreat = ntreat, ncat = ncat,nstudy = nstudy,
                     study = study, base.treat = base.treat, treat = treat, D = D, 
                     Prec.1 = prior$Prec.1, Prec.2 = prior$Prec.2, Prec.d = prior$Prec.d, Prec.Eta = prior$Prec.Eta, 
                     mean.d = prior$mean.d, mean.Eta = prior$mean.Eta)
            else
				inData <- list(narm = narm, ntreat = ntreat, ncat = ncat,nstudy = nstudy,
                     study = study, base.treat = base.treat, treat = treat, D = D, 
                     Prec.1 = prior$Prec.1, Prec.2 = prior$Prec.2, Prec.d = prior$Prec.d, Prec.Eta = prior$Prec.Eta, 
                     mean.d = prior$mean.d, mean.Eta = prior$mean.Eta,corr.factor=corr.factor)
            
        }
        else {
        	x <- as.matrix(dat[, covs])
        	ncov <- dim(x)[2]
            if (length(miss.patterns[[1]])==1)
                inData <- list(narm = narm, ntreat = ntreat, ncat = ncat, nstudy = nstudy, ncov=ncov,
                     study = study, base.treat = base.treat, treat = treat, D = D, x = x, 
                     Prec.1 = prior$Prec.1, Prec.2 = prior$Prec.2, Prec.d = prior$Prec.d, Prec.Eta = prior$Prec.Eta, Prec.slope=prior$Prec.slope,
                     mean.d = prior$mean.d, mean.Eta = prior$mean.Eta, mean.slope = prior$mean.slope)
            else
            	inData <- list(narm = narm, ntreat = ntreat, ncat = ncat, nstudy = nstudy, ncov=ncov,
                     study = study, base.treat = base.treat, treat = treat, D = D, x = x, 
                     Prec.1 = prior$Prec.1, Prec.2 = prior$Prec.2, Prec.d = prior$Prec.d, Prec.Eta = prior$Prec.Eta, Prec.slope=prior$Prec.slope,
                     mean.d = prior$mean.d, mean.Eta = prior$mean.Eta, mean.slope = prior$mean.slope, corr.factor=corr.factor)
        }
        bugsData(inData, paste(path, "data.txt", sep = ""))
}





multi.model <-
function (dat,miss.patterns,narm, ncat, model, prior,covs, var.model, varprior, path) 
{
# Inputs 
#        dat                 dataset (used if complete data)
#        miss.patterns       # missing data patterns
#        ncat               # outcome categories
#        model               baseline or constant
#		 var.model			 homogeneous, independent, consistent
	cat("model\n{\n",file=paste(path,"model.txt",sep=""))
#	if (is.null(miss.patterns)) multi.model.completedata(dat,narm,ncat,path)
#	else 
#	multi.model.missingdata(miss.patterns, ncat,path)
	if (length(miss.patterns[[1]])==1) complete = T
	else complete = F
	miss.patterns.nobs <- miss.patterns.nprobs <- integer(length(miss.patterns[[1]]))
	for (i in seq(length(miss.patterns[[1]]))) {
		miss.patterns.nprobs[i] <- length(miss.patterns[[1]][[i]][[2]]) #number of parameter combinations in ith pattern
		miss.patterns.nobs[i] <- length(miss.patterns[[1]][[i]][[1]])  # number of studies in ith pattern
	}
  
	for (i in seq(length(miss.patterns[[1]]))) {
		cat("\n   for (i in 1:", miss.patterns.nobs[i],") {\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		if (i==1) {
			cat("       D[i,1:", miss.patterns.nprobs[i], "] ~ dmulti(P",i,"[i,],N",i,"[i])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			cat("       N",i,"[i] <- sum(D[i,1:", miss.patterns.nprobs[i], "])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		}
		else {
			cat("       D[i+", cumsum(miss.patterns.nobs)[i-1], ",1:", miss.patterns.nprobs[i], "] ~ dmulti(P",i,"[i,],N",i,"[i])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			cat("       N",i,"[i] <- sum(D[i+", cumsum(miss.patterns.nobs)[i-1], ",1:", miss.patterns.nprobs[i], "])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		}
		for (j in seq(miss.patterns.nprobs[i]-1)) {
			combo <- miss.patterns[[2]][miss.patterns[[1]][[i]][[2]][j],]
			pos <- seq(ncat)[as.logical(abs(combo))]
			signs <- ifelse(combo[pos]==1,"+","-") 
			if (i==1) {
				if (length(pos)>1) {
					str <- paste(signs[-1],"P[i,",pos[-1],"]",sep="")
					if (complete) 
						cat("       P",i,"[i,", j, "] <- (P[i,",pos[1],"]",str,")\n",file = paste(path,"model.txt",sep=""), append = T, sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i,",pos[1],"]",str,")*corr.factor[i]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else
					if (complete)
						cat("       P",i,"[i,", j, "] <- P[i,",pos[1],"]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- P[i,",pos[1],"]*corr.factor[i]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
			}
			else {
				if (length(pos)>1) {
					str <- paste(signs[-1],"P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[-1],"]",sep="")
					if (complete)
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"]",str,")\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"]",str,")*corr.factor[i+",cumsum(miss.patterns.nobs)[i-1],"]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else
					if (complete)
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"])*corr.factor[i+",cumsum(miss.patterns.nobs)[i-1],"]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			}    
		}
		cat("       P",i,"[i,", miss.patterns.nprobs[i],"] <- 1 - sum(P",i,"[i,1:",miss.patterns.nprobs[i]-1,"])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		cat("for (k in 1:", miss.patterns.nprobs[i],")  {\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				if (i==1) {
					cat("Dhat[i,k] <- P",i,"[i,k]*N",i,"[i]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					cat("dev[i,k] <- 2*D[i,k]*(log(D[i,k])-log(Dhat[i,k]))\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else {
					cat("Dhat[i+", cumsum(miss.patterns.nobs)[i-1],",k] <- P",i,"[i,k]*N",i,"[i]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					cat("dev[i+",cumsum(miss.patterns.nobs)[i-1],",k] <- 2*D[i+",cumsum(miss.patterns.nobs)[i-1],",k]*(log(D[i+",cumsum(miss.patterns.nobs)[i-1],",k])-log(Dhat[i+",cumsum(miss.patterns.nobs)[i-1],",k]))\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
		cat("}\n}", file = paste(path,"model.txt",sep=""), append = T,sep="")
	}
	ncov <- length(covs)
	if (model == "baseline") {
		cat("
	for (i in 1:narm) {
		for (k in 1:ncat) {
			P[i,k] <- theta[i,k]/sum(theta[i,])        #baseline category 
			logit(theta[i,k]) <- Eta[study[i],k] + Delta[i,k] * (1 - equals(treat[i],base.treat[i]))
		}
		Delta[i,1] <- 0\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		if (ncat > 2) {
			if (var.model == "ind" | var.model == "consis") {
				cat("
				Delta[i,2:ncat] ~  dmnorm(Mu.Delta[i,], Prec[base.treat[i],treat[i],,])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
			else if (var.model == "hom") {
				cat("
				Delta[i,2:ncat] ~  dmnorm(Mu.Delta[i,], Prec[,])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
		}
		else if (ncat == 2) {
			if (var.model == "ind" | var.model == "consis") {
				cat("
				Delta[i,2] ~  dnorm(Mu.Delta[i], Prec[base.treat[i],treat[i]])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
			else if (var.model == "hom") {
				cat("
				Delta[i,2] ~  dnorm(Mu.Delta[i], Prec)\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
		}
		
	}
	else if (model == "logit") {
		cat("
	for (i in 1:narm) {
		for (k in 1:(ncat - 1)) {
			logit(P[i,k+1]) <- theta[i,k]
			theta[i,k] <- Eta[study[i],k] + Delta[i,k] * (1 - equals(treat[i],base.treat[i]))
		}\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		if (ncat > 2) {
			if (var.model == "ind" | var.model == "consis") {
				cat("
				Delta[i,1:(ncat-1)] ~ dmnorm(Mu.Delta[i,], Prec[base.treat[i],treat[i],,])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
			else if (var.model == "hom") {
				cat("
				Delta[i,1:(ncat-1)] ~ dmnorm(Mu.Delta[i,], Prec[,])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
		}
		else if (ncat == 2) {
			if (var.model == "ind" | var.model == "consis") {
				cat("
				Delta[i,1] ~  dnorm(Mu.Delta[i], Prec[base.treat[i],treat[i]])\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
			else if (var.model == "hom") {
				cat("
				Delta[i,1] ~  dnorm(Mu.Delta[i], Prec)\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			}
		}
	}
	if (ncat > 2) {
		base.string <- " 		    Mu.Delta[i,j] <- d[treat[i],j] - d[base.treat[i],j]"
		comment <- " #Mu.Delta has dim [narm x (ncat-1)]"
	}
	else if (ncat == 2) {
		base.string <- " 		    Mu.Delta[i] <- d[treat[i],j] - d[base.treat[i],j]"
		comment <- " #Mu.Delta has dim [narm x (ncat-1)]"
	}
	cat("
		for (j in 1:(ncat-1)){ \n",file=paste(path,"model.txt",sep=""),append=T,sep="")
			if (is.null(covs)) {
				cat(
				base.string,comment,file=paste(path,"model.txt",sep=""),append=T,sep="")
			}	
			else  {
				beta1.string <- base.string
				for (k in 1:ncov)
					beta1.string <- paste(beta1.string, " + Beta",k,"[i,j]*x[i,",k,"]",sep="")
				cat(
			    beta1.string,comment,file=paste(path,"model.txt",sep=""),append=T,sep="")
			    for (k in 1:ncov) {
			    	beta2.string <- paste("\n            Beta",k,"[i,j] <- slope[treat[i],",k,",j] - slope[base.treat[i],",k,",j]",sep="")
			    	cat(beta2.string, file=paste(path,"model.txt",sep=""),append=T,sep="")
			    }
			}
			cat("
      	}\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		if (model == "baseline") {
		cat("
	}
	for (i in 1:nstudy) {
		Eta[i, 2:ncat] ~ ", prior$Prior.Eta, "
		Eta[i, 1] <- 0
	}\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
		else if (model == "logit") {
			cat("
			P[i,1] <- 1 - sum(P[i,2:ncat])
	}
	for (i in 1:nstudy) {
		Eta[i,1:(ncat-1)] ~ ", prior$Prior.Eta, "
	}\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
		cat("
	for (k in 1:(ncat-1)) {
		d[1,k] <- 0\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		if (!is.null(covs)) {
		cat("
		for (j in 1:ncov) {
		   slope[1,j,k] <- 0
		}\n",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
		cat("
	}
	for (j in 2:ntreat) {
		d[j,1:(ncat-1)] ~ ", prior$Prior.d, "#d has dim [ntreat x (ncat-1)]", file=paste(path,"model.txt",sep=""),append=T,sep="")
		if (!is.null(covs)) {
		cat("
		for (k in 1:ncov) {
			slope[j,k,1:(ncat-1)] ~", prior$Prior.slope, 
		"}",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
		cat("
	}",file=paste(path,"model.txt",sep=""),append=T,sep="")
	if (var.model == "ind") multi.var.ind(ntreat,ncat,prior,path)
	else if (var.model == "hom") multi.var.hom(ntreat,ncat,prior,varprior,path)
	else if (var.model == "consis") multi.var.consis(ntreat,ncat,prior,path)
#   odds.ratio <- exp(d)
#   pred.LOR ~ dmnorm(d, Prec)
}


multi.model.completedata <-
function (data,narm,ncat,path) 
{
cat("\n   for (i in 1:", narm,") {\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
cat("       D[i,1:", ncat, "] ~ dmulti(P[i,],N[i])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
cat("       N[i] <- sum(D[i,1:", ncat, "])\n	}\n", file = paste(path,"model.txt",sep=""), append = T,sep="")

}


multi.model.missingdata <-
function (miss.patterns, ncat,path) 
{
	if (length(miss.patterns[[1]])==1) complete = T
	miss.patterns.nobs <- miss.patterns.nprobs <- integer(length(miss.patterns[[1]]))
	for (i in seq(length(miss.patterns[[1]]))) {
		miss.patterns.nprobs[i] <- length(miss.patterns[[1]][[i]][[2]]) #number of parameter combinations in ith pattern
		miss.patterns.nobs[i] <- length(miss.patterns[[1]][[i]][[1]])  # number of studies in ith pattern
	}
  
	for (i in seq(length(miss.patterns[[1]]))) {
		cat("\n   for (i in 1:", miss.patterns.nobs[i],") {\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		if (i==1) {
			cat("       D[i,1:", miss.patterns.nprobs[i], "] ~ dmulti(P",i,"[i,],N",i,"[i])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			cat("       N",i,"[i] <- sum(D[i,1:", miss.patterns.nprobs[i], "])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		}
		else {
			cat("       D[i+", cumsum(miss.patterns.nobs)[i-1], ",1:", miss.patterns.nprobs[i], "] ~ dmulti(P",i,"[i,],N",i,"[i])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			cat("       N",i,"[i] <- sum(D[i+", cumsum(miss.patterns.nobs)[i-1], ",1:", miss.patterns.nprobs[i], "])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		}
		for (j in seq(miss.patterns.nprobs[i]-1)) {
			combo <- miss.patterns[[2]][miss.patterns[[1]][[i]][[2]][j],]
			pos <- seq(ncat)[as.logical(abs(combo))]
			signs <- ifelse(combo[pos]==1,"+","-") 
			if (i==1) {
				if (length(pos)>1) {
					str <- paste(signs[-1],"P[i,",pos[-1],"]",sep="")
					if (complete) 
						cat("       P",i,"[i,", j, "] <- (P[i,",pos[1],"]",str,")\n",file = paste(path,"model.txt",sep=""), append = T, sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i,",pos[1],"]",str,")*corr.factor[i]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else
					if (complete)
						cat("       P",i,"[i,", j, "] <- P[i,",pos[1],"]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- P[i,",pos[1],"]*corr.factor[i]\n",file = paste(path,"model.txt",sep=""), append = T,sep="")
			}
			else {
				if (length(pos)>1) {
					str <- paste(signs[-1],"P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[-1],"]",sep="")
					if (complete)
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"]",str,")\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"]",str,")*corr.factor[i+",cumsum(miss.patterns.nobs)[i-1],"]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else
					if (complete)
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					else
						cat("       P",i,"[i,", j, "] <- (P[i+", cumsum(miss.patterns.nobs)[i-1],",",pos[1],"])*corr.factor[i+",cumsum(miss.patterns.nobs)[i-1],"]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
			}    
		}
		cat("       P",i,"[i,", miss.patterns.nprobs[i],"] <- 1 - sum(P",i,"[i,1:",miss.patterns.nprobs[i]-1,"])\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
		cat("for (k in 1:", miss.patterns.nprobs[i],")  {\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				if (i==1) {
					cat("Dhat[i,k] <- P",i,"[i,k]*N",i,"[i]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					cat("dev[i,k] <- 2*D[i,k]*(log(D[i,k])-log(Dhat[i,k]))\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
				else {
					cat("Dhat[i+", cumsum(miss.patterns.nobs)[i-1],",k] <- P",i,"[i,k]*N",i,"[i]\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
					cat("dev[i+",cumsum(miss.patterns.nobs)[i-1],",k] <- 2*D[i+",cumsum(miss.patterns.nobs)[i-1],",k]*(log(D[i+",cumsum(miss.patterns.nobs)[i-1],",k])-log(Dhat[i+",cumsum(miss.patterns.nobs)[i-1],",k]))\n", file = paste(path,"model.txt",sep=""), append = T,sep="")
				}
		cat("}\n}", file = paste(path,"model.txt",sep=""), append = T,sep="")
	}
}


multi.var.hom <- function(ntreat,ncat,prior,varprior,path)
{
	if (ncat > 2) {
		cat("
		Prec[1:(ncat-1),1:(ncat-1)] ~ ",prior$Prior.Prec,"
		Sigma[1:(ncat-1),1:(ncat-1)] <- inverse(Prec[,])
		}",file=paste(path,"model.txt",sep=""),append=T,sep="")
	}
	else {
		if (varprior[[1]]=="prec") {
			cat("
			Prec ~ ",prior$Prior.Prec,"
			Sigma <- 1/Prec
			}",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
		else if (varprior[[1]] == "sd") {
			cat("
			Sd ~ ",prior$Prior.Prec,"
			Prec <- 1/pow(Sd,2)
			}",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
	}
}

multi.var.ind <- function (ntreat,ncat,prior,path)
{
# Prec has dim [(ntreat-1) x ntreat x (ncat-1) x (ncat-1)]
	if (ncat > 2) {
		cat("
		for (i in 1:(ntreat-1)) {
			for (j in 1:(ncat-1)) {
				Prec[i,i,j,j] <- 1
				Sigma[i,i,j,j] <- 1
				for (k in 1:(j-1)) {
					Prec[i,i,j,k] <- 0
					Sigma[i,i,j,k] <- 0
				}
				for (k in (j+1):(ncat-1)) {
					Prec[i,i,j,k] <- 0
					Sigma[i,i,j,k] <- 0
				}
			}
			for (j in (i+1):ntreat) {
				Prec[i,j,1:(ncat-1),1:(ncat-1)] ~",prior$Prior.Prec,"
				Sigma[i,j, 1:(ncat-1),1:(ncat-1)] <- inverse(Prec[i,j,,])
			}",file=paste(path,"model.txt",sep=""),append=T,sep="")
		}
	else {
		cat("
		for (i in 1:(ntreat-1)) {
			Sigma[i,i] <- 1
			Prec[i,i] <- 1
			for (j in (i+1):ntreat){
				Prec[i,j] ~", prior$Prior.Prec,"
				Sigma[i,j] <- 1/Prec[i,j]
			}",file=paste(path,"model.txt",sep=""),append=T,sep="")
        }
cat("
	}
}",file=paste(path,"model.txt",sep=""),append=T,sep="")

}

multi.var.consis <- function(ntreat,ncat,prior,path)
{
	if (ncat > 2) {
		cat("
		for(i in 1:(ntreat-1)){
			for (j in 1:(ncat-1)) {
				Prec[i,i,j,j] <- 1
				Sigma[i,i,j,j] <- 1
				for (k in 1:(j-1)) {
					Prec[i,i,j,k] <- 0
					Sigma[i,i,j,k] <- 0
				}
				for (k in (j+1):(ncat-1)) {
					Prec[i,i,j,k] <- 0
					Sigma[i,i,j,k] <- 0
				}
			}
			for (j in (i+1):ntreat) {
				Sigma[j,i,1:(ncat-1),1:(ncat-1)] <- Sigma[i,j,1:(ncat-1),1:(ncat-1)]
				Prec[i,j, 1:(ncat-1),1:(ncat-1)] <- inverse(Sigma[i,j,,])
				Prec[j,i,1:(ncat-1),1:(ncat-1)] <- Prec[i,j,1:(ncat-1),1:(ncat-1)]
			}
		}
		for (k in 1:(ncat-1)) {
			for(i in 1:ntreat){
				tau.omega[i,k] ~", prior$Prior.Prec,"  #dgamma(0.001,0.001)
				v.omega[i,k] <- 1/tau.omega[i,k]
				sd.omega[i,k] <- sqrt(v.omega[i,k]) 
			}
			pi.half <- 1.5708	
			for(i in 1:(ntreat-1)){
				for(j in (i+1):ntreat){
					L[j,i,k] <- 0
					phi[i,j,k] ~ dunif(0, pi.half)
					rho[i,j,k] <- inprod(L[, i,k],L[, j,k])
					Sigma[i,j,k,k] <- v.omega[i,k]+v.omega[j,k]-2*rho[i,j,k]*sd.omega[i,k]*sd.omega[j,k] 
				}
			}
			L[1,1,k] <- 1
			for (i in 2:ntreat) {
				L[1,i,k] <- cos(phi[1,i,k])
				L[i,i,k] <- 1
				for (j in 1:(i-1)) {
					L[i,i,k] <- L[i,i,k]*sin(phi[j,i,k])
				}
				for (j in (i+1):ntreat) {
					L[i,j,k] <- cos(phi[i,j,k])
					for (m in 1:(i-1)) {
						L[i,j,k] <- L[i,j,k] * sin(phi[m,j,k])
					}
				}
			}	
		}	
			
			",file=paste(path,"model.txt",sep=""),append=T,sep="")
	}
	else {
		cat("
		for(i in 1:(ntreat-1){
			Prec[i,i] <- 1
			Sigma[i,i] <- 1
			for(j in (i+1):ntreat){
				Sigma[j,i] <- Sigma[i,j]
				Prec[i,j] <- 1/Sigma[i,j]
				Prec[j,i] <- Prec[i,j] 
			}
		}
		for(i in 1:ntreat){
			tau.omega[i] ~", prior$Prior.Prec,"  #dgamma(0.001,0.001)
			v.omega[i] <- 1/tau.omega[i]
			sd.omega[i] <- sqrt(v.omega[i]) 
		}
		pi.half <- 1.5708
		for(i in 1:(ntreat-1)){
			for(j in (i+1):(ntreat){
				L[j,i] <- 0
				phi[i,j] ~ dunif(0, pi.half)
				rho[i,j] <- inprod(L[, i],L[, j])
				Sigma[i,j] <- v.omega[i]+v.omega[j]-2*rho[i,j]*sd.omega[i]*sd.omega[j] 
			}
		}
		L[1,1] <- 1
		for (i in 2:ntreat) {
			L[1,i] <- cos(phi[1,i])
			L[i,i] <- 1
			for (j in 1:(i-1)) {
				L[i,i] <- L[i,i]*sin(phi[j,i])
			}
			for (j in (i+1):ntreat) {
				L[i,j] <- cos(phi[i,j])
				for (k in 1:(i-1)) {
					L[i,j] <- L[i,j] * sin(phi[k,j])
				}
			}
		}	
	",file=paste(path,"model.txt",sep=""),append=T,sep="")
	}
	cat("
}",file=paste(path,"model.txt",sep=""),append=T,sep="")
}


multi.bugs.fit <-
function(model.file,data.file,numChains,inits.files,path,pars.to.save,setsize,conv.limit,niters,nruns,covs)
{
modelCheck(model.file)              # check model file
modelData("data.txt")                # read data file
modelCompile(numChains=3)            # compile model with 3 chains
modelInits(inits.files[1])              # read init data file
modelInits(inits.files[2])              # read init data file
modelInits(inits.files[3])              # read init data file
	samplesSet(pars.to.save)                # parameters to monitor
    i <- 0
    max.bgrRatio <- 10
    while (i < floor(niters/setsize) & max.bgrRatio > conv.limit) {	
		modelUpdate(setsize)                    # setsize number of iterations
		i <- i + 1
		d.bgr <- matrix(unlist(samplesBgr("d",1,setsize*i,plot=F,bins=2)),ncol=8,byrow=T)[,8]
    	Eta.bgr <- matrix(unlist(samplesBgr("Eta",1,setsize*i,plot=F,bins=2)),ncol=8,byrow=T)[,8]
		if(!is.null(covs)) slope.bgr <- matrix(unlist(samplesBgr("slope",1,setsize*i,plot=F,bins=2)),ncol=8,byrow=T)[,8]
		if(is.null(covs)) 
			max.bgrRatio <- max(c(d.bgr,Eta.bgr))
		else 
			max.bgrRatio <- max(c(d.bgr,slope.bgr,Eta.bgr))
		print(max.bgrRatio)
    	print(d.bgr)
    	if (!is.null(covs)) print(slope.bgr)
		print(Eta.bgr)
#		if(max(current.bgrRatio) <= conv.limit) {
#			no.to.converge <- i*setsize
#			break()
#		}
	}
	no.to.converge <- i*setsize
	no.to.keep <- no.to.converge/2.0      # 2nd half of converged sequence is good
	if (nruns > no.to.keep) {
	   samplesClear("*")
	   samplesSet(pars.to.save)
#	   dicSet()
	   modelUpdate(nruns)
	   }
	else {
	   thin <- floor(no.to.keep/nruns)
	   samplesClear("*")
	   samplesSet(pars.to.save)
#	   dicSet()
	   modelUpdate(no.to.keep,thin=thin)
	}
	if (is.null(covs))
		params <- c(samplesMonitors("d"),samplesMonitors("Prec"),samplesMonitors("P"))
	else
		params <- c(samplesMonitors("d"),samplesMonitors("slope"),samplesMonitors("Prec"),samplesMonitors("P")) 
	samples <- sapply(params, samplesSample)
	samples.array <- array(samples, c(nrow(samples)/numChains, numChains, ncol(samples)))
	dimnames(samples.array)[[3]] <- dimnames(samples)[[2]]
	mat<-samplesStats("*",beg=2+no.to.keep,end=modelIteration())       # summarized results
#	dic<-dicStats()
#	out<-list(no.to.converge,dim(samples.array)[1],mat,dic,samples.array)
#	names(out) <- c("BurnIn","No. Runs Per Chain", "Stats","DIC","Samples")
	out<-list(no.to.converge,dim(samples.array)[1],mat,samples.array)
	names(out) <- c("BurnIn","No. Runs Per Chain", "Stats","Samples")
	return(out)
}

rank.tx = function(x)
{
# Produces treatment ranking tables for each outcome category. Row is the rank and column is the treatment. 
# Entry gives the probability that column treatment has row rank
# x is output from model for d #sims x(ntreat-1)x(ncat-1)
	ncat = dim(x)[3] + 1
	ntreat = dim(x)[2] + 1
	nsim = dim(x)[1]
	rank.d = d.expand = array(NA,c(0,1,0) + dim(x))
	d.expand[,1,] = 0
	d.expand[,2:dim(d.expand)[2],] = x
	rank.table = array(NA,c(ntreat,ntreat,ncat-1))
	for (i in seq(nsim))
		rank.d[i,,] = apply(d.expand[i,,],2,rank)
	for (i in seq(ncat-1))
		for (j in seq(ntreat))
			rank.table[,j,i] = table(rank.d[,j,i])/nsim
	dimnames(rank.table) = list(NULL,paste("Treatment",seq(ntreat),sep=" "),paste("Category",seq(ncat-1),sep=" "))
	return(rank.table)
}

plot.rank.tx = function(rank.table,catnames,txnames)
{
#Plots probability that each treatment is in specific rank
#rank.table is ranking table produced by function rank.tx
#catnames are names of outcome categories
#txnames are names of treatments
	ntreat = dim(rank.table)[1]
	ncat = dim(rank.table)[3]
	if (missing(catnames)) catnames = paste("Outcome Category",seq(ncat),sep=" ")
	if (missing(txnames)) txnames = paste("Treatment",seq(ncat),sep=" ")
	ncol = floor(sqrt(ncat))
	nrow = ceiling(ncat/ncol)
	oldpar = par(no.readonly=T)
	par(mfrow = c(nrow, ncol))
	for (i in seq(ncat)) {
		plot(seq(ntreat),seq(ntreat),type="n",xaxt="n",ylim=c(0,1),yaxt="n",ylab="Probability",xlab="Rank")
		axis(side=1,at=seq(ntreat))
		axis(side=2,at=seq(0,1,by=0.2))
		title(catnames[i])
		for (j in seq(ntreat))
			points(seq(ntreat), rank.table[,j,i],type="l",lty=j,col=j)
		legend(1,1,txnames,lty=1:4,bty="n",cex=.75)
	}
	par(oldpar,no.readonly=T)
}

plot.cumrank.tx = function(rank.table,catnames,txnames)
{
#Plots cumulative probability of ranks for each treatment
#rank.table is ranking table produced by function rank.tx
#catnames are names of outcome categories
#txnames are names of treatments
	ntreat = dim(rank.table)[1]
	ncat = dim(rank.table)[3]
	if (missing(catnames)) catnames = paste("Outcome Category",seq(ncat),sep=" ")
	if (missing(txnames)) txnames = paste("Treatment",seq(ncat),sep=" ")
	ncol = floor(sqrt(ncat))
	nrow = ceiling(ncat/ncol)
	oldpar = par(no.readonly=T)
	par(mfrow = c(nrow, ncol))
	for (i in seq(ncat))  {
		x = apply(rank.table[,,i],2,cumsum)
		plot(seq(ntreat),seq(ntreat),type="n",xaxt="n",ylim=c(0,1),yaxt="n",ylab="Cumulative Probability",xlab="Rank")
		axis(side=1,at=seq(ntreat))
		axis(side=2,at=seq(0,1,by=0.2))
		title(catnames[i])
		for (j in seq(ntreat))
			points(seq(ntreat), x[,j],type="l",lty=j,col=j)
		legend(1,1,txnames,lty=1:4,bty="n",cex=.75)
	}
	par(oldpar,no.readonly=T)
}

sucra = function(rank.table,catnames,txnames)
{
#mulit.
#rank.table is ranking table produced by function rank.tx
#catnames are names of outcome categories
#txnames are names of treatments
	ncat = dim(rank.table)[3]
	ntreat = dim(rank.table)[1]
	if (missing(catnames)) catnames = paste("Outcome Category",seq(ncat),sep=" ")
	if (missing(txnames)) txnames = paste("Treatment",seq(ncat),sep=" ")
	x = array(NA,dim(rank.table)[2:3])
	for (i in seq(ncat))
		x[,i] = apply(apply(rank.table[-ntreat,,i],2,cumsum),2,sum)/(ntreat-1)
	dimnames(x) = list(txnames,catnames)
	return(x)
}

resid.diag = function(mod)
# Graph leverage vs. residual deviance with curves of fit
# Then returns summary statistics
# mod            model for which to compute diagnostics
{
	Nsize = mod$N    # study sample sizes
	ncat = mod$No.Categories   # No. outcome categories
	miss.patterns = mod$Missing.Data.Patterns    # missing data patterns
	Data = mod$Data  #data frame with all outcomes
	post.dev = mod$Stats[grep("dev",dimnames(mod$Stats)[[1]]),1]    #mean posterior residual deviance (#trials x #arms)
	x1 = grep("P",dimnames(mod$Stats)[[1]])
	x2 = grep("Prec",dimnames(mod$Stats)[[1]])
	x1[!x1%in%x2]
	P = matrix(mod$Stats[x1[!x1%in%x2],1],ncol=ncat,byrow=T)  # posterior means of p[i,k]
	x <- seq(-3, 3, 0.1)
	y1 <- 1 - x^2
	y2 <- 2 - x^2
	y3 <- 3 - x^2
# Convert complete posterior probabilities to incomplete ones
	miss.mat = miss.patterns[[2]]  #association of data variables to complete outcomes
	N = D = post.prob = rep(NA,length(post.dev))
	count = 0
	npatterns = length(miss.patterns[[1]])
	for (i in seq(npatterns)) {
		vars = miss.patterns[[1]][[i]][[2]]  # variables in ith missing data pattern
		arms = miss.patterns[[1]][[i]][[1]]  # arms in ith missing data pattern
		D[count+seq(length(vars)*length(arms))] = t(Data[miss.patterns[[1]][[i]][[1]],miss.patterns[[1]][[i]][[2]]])
		N[count+seq(length(vars)*length(arms))] = rep(Nsize[arms],each=length(vars))
		post.prob[count+seq(length(vars)*length(arms))] = as.vector(miss.mat[vars,]%*%t(P[arms,]))
		count = count + length(vars)*length(arms)		
	}
	D.post = 2*D*(log(D)-log(post.prob*N))
	pD = post.dev - D.post
	count = 0
	pD.arm = post.dev.arm = rep(NA,dim(Data)[1])
	for (i in seq(npatterns)) {
		arms = miss.patterns[[1]][[i]][[1]]
		narms = length(arms)
		nvars = length(miss.patterns[[1]][[i]][[2]])
		for (j in seq(narms)) {
			post.dev.arm[arms[1]-1+j] = sum(post.dev[count+seq(nvars)])
			pD.arm[arms[1]-1+j] = sum(pD[count+seq(nvars)])
			count = count + nvars
		}
	}
	plot(sign(post.dev.arm)*sqrt(abs(post.dev.arm)), pD.arm, pch = 1, main = "Fit of the model", xlab = "Residual Deviance (postDi)", ylab = "Leverage (pDi)")
    matlines(x, cbind(y1, y2, y3))
    resid.dev = sum(post.dev)
	eff.no.params = sum(pD)
	DIC = resid.dev + eff.no.params
	out = list(round(post.dev,2), round(pD,2), post.dev.arm, pD.arm,resid.dev,length(D),eff.no.params,DIC)
	names(out) = c("Residual deviances","Leverages","Arm Deviances","Arm Leverages","Total Residual Deviance","Number of data points","Effective number of parameters","DIC")
	return(out)
}


make.tx.comparison.table <-
function (x) 
{
# For each outcome category, produces summary statistics comparing treatment i to j. First row gives means for # each treatment.
# x is coda output for d #sims x(ntreat-1)x(ncat-1)
ncat<-dim(x)[3] + 1
ntreat <- dim(x)[2] + 1
mean.x <- sd.x <- median.x <- p025.x <- p975.x <- array(NA,c(ntreat,ntreat,ncat-1))
dimnames(mean.x) = list(paste("Tx ",seq(ntreat)),paste("Tx ",seq(ntreat)),paste("Category ",seq(ncat-1)))
dimnames(sd.x) = dimnames(median.x) = dimnames(p025.x) = dimnames(p975.x) = dimnames(mean.x)
for (k in 1:(ncat-1)) {
	for (i in 2:ntreat) {
		mean.x[1,i,k] <- mean(x[,i-1,k],na.rm=T)
		sd.x[1,i,k] <- sd(x[,i-1,k],na.rm=T)
		median.x[1,i,k] <- median(x[,i-1,k],na.rm=T)
		p025.x[1,i,k] <- quantile(x[,i-1,k],.025,na.rm=T)
		p975.x[1,i,k] <- quantile(x[,i-1,k],.975,na.rm=T)
		for (j in i:ntreat) {
			if (j > i) {
				mean.x[i,j,k] <- mean(x[,j-1,k]-x[,i-1,k],na.rm=T)
				sd.x[i,j,k] <- sd(x[,j-1,k]-x[,i-1,k],na.rm=T)
				median.x[i,j,k] <- median(x[,j-1,k]-x[,i-1,k],na.rm=T)
				p025.x[i,j,k] <- quantile(x[,j-1,k]-x[,i-1,k],.025,na.rm=T)
				p975.x[i,j,k] <- quantile(x[,j-1,k]-x[,i-1,k],.975,na.rm=T)
			}
		}
	}
}
out<-list(mean.x,sd.x,exp(median.x),exp(p025.x),exp(p975.x))
names(out)<-c("Mean","SD","Median","P025","P975")
return(out)
}


multi.stats <-
function(x) {
last.dim <- length(dim(x))
margins<-seq(2,last.dim)
mean.x<-apply(x,margins,mean,na.rm=T)
median.x<-apply(x,margins,median,na.rm=T)
sd.x<-apply(x,margins,sd,na.rm=T)
p025.x<-apply(x,margins,quantile,.025,na.rm=T)
p975.x<-apply(x,margins,quantile,.975,na.rm=T)
out<-list(mean.x,sd.x,median.x,p025.x,p975.x)
names(out)<-c("Mean","SD","Median","P025","P975")
return(out)
}


sort.vec <-
function(x)
{
# Re-sorts vector x in order starting with one
	old.x <- x
    sorted.x <- sort(unique(old.x))
    nx <- length(old.x)
    x <- rep(NA, nx)
    for (i in 1:nx) x[old.x == sorted.x[i]] <- i #relabel studies in numerical order starting with one
    return(x)
}

invlogit <-
function(x){exp(x)/(1+exp(x))}


logit <-
function(p){log(p/(1-p))}

unit.test <-
function () 
{

run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("2\n\n\n\n")
run.multi(lipids.data[1:34,],c(8,10), ,lipids.data[1:34,11],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("3\n\n\n\n")
run.multi(lipids.data[1:8,], 5:10,,lipids.data[1:8,14], conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("4\n\n\n\n")
run.multi(lipids.data[1:18,],5:10,,lipids.data[1:18,14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("5\n\n\n\n")
run.multi(lipids.data[c(1:8,19:22),],5:14,misspatterns2.multi,lipids.data[c(1:8,19:22),14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("6\n\n\n\n")
run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("7\n\n\n\n")
run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("8\n\n\n\n")
run.multi(lipids.data[1:34,],c(8,10), ,lipids.data[1:34,11],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("9\n\n\n\n")
run.multi(lipids.data[1:8,],5:10,,lipids.data[1:8,14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("10\n\n\n\n")
run.multi(lipids.data[1:18,],5:10,,lipids.data[1:18,14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("11\n\n\n\n")
run.multi(lipids.data[c(1:8,19:22),],5:14,misspatterns2.multi,lipids.data[c(1:8,19:22),14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("12\n\n\n\n")
run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/")
cat("13\n\n\n\n")
run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=4)
cat("14\n\n\n\n")
run.multi(lipids.data[1:34,],c(8,10), ,lipids.data[1:34,11],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=4)
cat("15\n\n\n\n")
run.multi(lipids.data[c(1:8,15,16,19:22,27,28),], c(8,10,12),,lipids.data[c(1:8,15,16,19:22,27,28),8] + lipids.data[c(1:8,15,16,19:22,27,28),10] + lipids.data[c(1:8,15,16,19:22,27,28),12], conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=4)
cat("16\n\n\n\n")
run.multi(lipids.data[1:32,],c(8,10,12),,lipids.data[1:32,8]+lipids.data[1:32,10]+lipids.data[1:32,12],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/")
cat("17\n\n\n\n")
run.multi(lipids.data[c(1:8,19:22),],5:14,misspatterns2.multi,lipids.data[c(1:8,19:22),14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=4)
cat("18\n\n\n\n")
run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=4)
cat("19\n\n\n\n")
run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("20\n\n\n\n")
run.multi(lipids.data[1:34,], c(8,10), ,lipids.data[1:34,11], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("21\n\n\n\n")
run.multi(lipids.data[c(1:8,15,16,19:22,27,28),], c(8,10,12),,lipids.data[c(1:8,15,16,19:22,27,28),8] + lipids.data[c(1:8,15,16,19:22,27,28),10] + lipids.data[c(1:8,15,16,19:22,27,28),12], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("22\n\n\n\n")
run.multi(lipids.data[1:32,],c(8,10,12),,lipids.data[1:32,8]+lipids.data[1:32,10]+lipids.data[1:32,12],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("23\n\n\n\n")
run.multi(lipids.data[c(1:8,15,16,19:22,27,28),],5:14,misspatterns2.multi,lipids.data[c(1:8,15,16,19:22,27,28),14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("24\n\n\n\n")
run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=4)
cat("25\n\n\n\n")
run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("26\n\n\n\n")
run.multi(lipids.data[1:34,],c(8,10), ,lipids.data[1:34,11],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("27\n\n\n\n")
run.multi(lipids.data[c(1:8,15,16,19:22,27,28),], c(8,10,12),,lipids.data[c(1:8,15,16,19:22,27,28),8] + lipids.data[c(1:8,15,16,19:22,27,28),10] + lipids.data[c(1:8,15,16,19:22,27,28),12], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("28\n\n\n\n")
run.multi(lipids.data[1:32,],c(8,10,12),,lipids.data[1:32,8]+lipids.data[1:32,10]+lipids.data[1:32,12],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("29\n\n\n\n")
run.multi(lipids.data[c(1:8,19:22),],5:14,misspatterns2.multi,lipids.data[c(1:8,19:22),14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("30\n\n\n\n")
#run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="baseline",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("31\n\n\n\n")
run.multi(lipids.data[1:8,], c(8,10), ,lipids.data[1:8,11], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("32\n\n\n\n")
run.multi(lipids.data[1:34,],c(8,10), ,lipids.data[1:34,11],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("33\n\n\n\n")
run.multi(lipids.data[c(1:8,15,16,19:22,27,28),], c(8,10,12),,lipids.data[c(1:8,15,16,19:22,27,28),8] + lipids.data[c(1:8,15,16,19:22,27,28),10] + lipids.data[c(1:8,15,16,19:22,27,28),12], conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("34\n\n\n\n")
run.multi(lipids.data[1:32,],c(8,10,12),,lipids.data[1:32,8]+lipids.data[1:32,10]+lipids.data[1:32,12],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("35\n\n\n\n")
run.multi(lipids.data[c(1:8,19:22),],5:14,misspatterns2.multi,lipids.data[c(1:8,19:22),14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("36\n\n\n\n")
#run.multi(lipids.data,5:14,misspatterns.multi,lipids.data[,14],conv.limit=1.2,nruns=5000,model="logit",path="e:/multiple treatments/multinomial/",covs=c(2,4))
cat("37\n\n\n\n")
}

