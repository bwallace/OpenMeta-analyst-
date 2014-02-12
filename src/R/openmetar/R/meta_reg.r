##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  George E. Dietz
#  Brown CEBM
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta_reg.r                                                    #
##################################################################

library(metafor)

#gfactor <- function(x, ref.value=NULL) {
#	### Transforms x in to a factor, with ref.value being the first level ###
#	
#	# Just set reference value to be the first value if ref.value not specified
#	if (is.null(ref.value)) {
#		ref.value <- x[1]
#	}
#	
#	# sort levels, sticking ref.value at the front
#	levels <- unique(x)
#	levels.without.ref.value <- levels[levels!=ref.value]
#	levels <- c(ref.value, sort(levels.without.ref.value))
#	
#	factor(x, levels=levels)
#}


regression.wrapper <- function(data, mods.str, method, level, digits, btt=NULL) {
	# Construct call to rma
	call_str <- sprintf("rma.uni(yi,vi, mods=%s, data=data, method=\"%s\", level=%f, digits=%d)", mods.str, method, level, digits)
	cat(call_str,"\n")
	expr<-parse(text=call_str) # convert to expression
	res <- eval(expr) # evaluate expression
	res
}

make.mods.str <-function(mods) {
	# Builds the mods string as specified by the information in mods
	# The order will be numeric, categorical, then interaction moderators
	# factors in data already assumed to be factors with ref.value set as first
	#     level
	
	str.els <- c() # mods string elements
	
	# numeric 
	for (mod in mods[["numeric"]]) {
		str.els <- c(str.els, mod)
	}
	
	# categorical
	for (mod in mods[["categorical"]]) {
		str.els <- c(str.els, mod)
	}
	
	# interactions
	for (interaction in names(mods[['interactions']])) {
		str.els <- c(str.els, interaction)
	}
		

	mods.str <- paste("~", paste(str.els,collapse=" + "), sep=" ")
	cat(mods.str,"\n")
	mods.str
}

make.design.matrix <- function(strat.cov, mods, cond.means.data, data) {
	# Make design matrix for conditional means
	# strat.cov is the name of the covariate in data to stratify over
	# This code is very sensitive to the fact that when there is an interaction
	# of the form A:B, the coefficients are given such that the A coefficients iterate
	# before the B coefficients
	
	nlevels <- length(levels(data[[strat.cov]])) # num of levels in strat.cov
	rownames <- levels(data[[strat.cov]])
	colnames <- c("Intercept")
	dsn.matrix <- matrix(rep(1,nlevels)) # intercept column
	
	# 1 column for each numeric moderator
	for (mod in mods[["numeric"]]) {
		value <- cond.means.data[[mod]]
		dsn.matrix <- cbind(dsn.matrix,rep(value,nlevels))
		colnames<-c(colnames, mod)
	}
	
	### NOTE: In following code mod.matrix is the part of the matrix to be stuck
	###       on to dsn.matrix corresponding to a categorical moderator or
	###       an interaction
	# qi-1 columns for each categorical modertor where q is the # of levels of
	# the moderator
	for (mod in mods[["categorical"]]) {
		l.mod <- levels(data[[mod]]) # levels of the moderator
		mod.matrix <- c()
		
		if (mod==strat.cov) {
			# iterate over the levels of the moderator
			for (x in l.mod) {
				x.coded <- coded.cat.mod.level(x, l.mod)
				mod.matrix <- rbind(mod.matrix,x.coded)
			}
		} else {
			# just replicate the coding of the desired level
			value <- cond.means.data[[mod]]
			lvl.coded <- coded.cat.mod.level(value, l.mod)
			for (x in 1:nlevels) {
				mod.matrix <- rbind(mod.matrix, lvl.coded)
			}
		} # end of else

		dsn.matrix <- cbind(dsn.matrix, mod.matrix)
		colnames<-c(colnames, paste(mod, l.mod[2:length(l.mod)],sep=""))
	} # end for categorical
	
	
	# interactions
	for (interaction in names(mods[['interactions']])) {
		interaction.vars <- mods[['interactions']][[interaction]]
		# What type of interaction? CAT:CAT, CAT:CONT, or CONT:CONT?
		
		cat.cat <- (interaction.vars[1] %in% mods[['categorical']]) && (interaction.vars[2] %in% mods[['categorical']])
		# same thing
		cat.cont <- (interaction.vars[1] %in% mods[['categorical']]) && (interaction.vars[2] %in% mods[['numeric']])
		cont.cat <- (interaction.vars[1] %in% mods[['numeric']]) && (interaction.vars[2] %in% mods[['categorical']])
		cat.cont <- cat.cont || cont.cat
		overall.mod.matrix <- c()
		
		if (cat.cat) {
			# two categorical variables Note: (p-1)*(q-1) columns where p and q
		    # are the # of levels in the first categorical var and the 2nd
		    # respectively
			
			cat1.levels <- levels(data[[interaction.vars[1]]])
			cat2.levels <- levels(data[[interaction.vars[2]]])
			
			if (strat.cov %in% interaction.vars) {
				# One of the variables in the interaction is the stratification variable		
				strat.cov.is.first <- strat.cov ==  interaction.vars[1]
				if (strat.cov.is.first) {
					# iterate over levels of first cov, keeping 2nd cov level constant
					value2 <- cond.means.data[[interaction.vars[2]]]
					mod.matrix <- c()
					for (value1 in cat1.levels) {
						row.vector <- get.row.vector.cat.cat(
								cat1.levels, cat2.levels,
								value1, value2)
						mod.matrix <- rbind(mod.matrix,row.vector)
					}
				}
				else {
					# strat.cov is 2nd
					# Iterate over levels of 2nd cov, keeping the 1st cov level
				    # constant.
					value1 = cond.means.data[[interaction.vars[1]]]
					mod.matrix <- c()
					for (value2 in cat2.levels) {
						row.vector <- get.row.vector.cat.cat(
								cat1.levels, cat2.levels,
								value1, value2)
						mod.matrix <- rbind(mod.matrix,row.vector)
					}
				}
			} else {
				# Neither of the variables in the interaction is the stratification variable
				value1 = cond.means.data[[interaction.vars[1]]]
				value2 = cond.means.data[[interaction.vars[2]]]
				
				row.vector <- get.row.vector.cat.cat(
						cat1.levels, cat2.levels,
						value1, value2)
				
				mod.matrix <- c()
				for (i in 1:nlevels) {
					mod.matrix <- rbind(mod.matrix,row.vector)
				}
			} # end else strat.cov cat:cat
			
			### Generate column labels
			# names of interaction vars
			intvar1 <- interaction.vars[1]
			intvar2 <- interaction.vars[2]
			col.names.for.interaction <- c()
			for (y in cat2.levels[2:length(cat2.levels)]) {
				for (x in cat1.levels[2:length(cat1.levels)]) {
					col.names.for.interaction <- c(col.names.for.interaction, paste(intvar1, x, ":",intvar2,y,sep=""))
				}
			}
			colnames<-c(colnames, col.names.for.interaction)
		} else if (cat.cont) {
			# one categorical, one continuous # (p-1) columns
			if (strat.cov %in% interaction.vars) {
				# One of the variables in the interaction is the stratification variable
				if (strat.cov==interaction.vars[1]) {
					strat.levels <- levels(data[[interaction.vars[1]]])
					cont.val <- cond.means.data[[interaction.vars[2]]]
				} else {
					strat.levels <- levels(data[[interaction.vars[2]]])
					cont.val <- cond.means.data[[interaction.vars[1]]]
				}
				
				mod.matrix <- c()
				for (x in strat.levels) {
					row.vector <- get.row.vector.cat.cont(strat.levels, x, cont.val)
					mod.matrix <- rbind(mod.matrix,row.vector)
				}
				
		    } else {
             	# Neither of the variables in the interaction is the stratification variable
                value1 <- cond.means.data[[interaction.vars[1]]]
                value2 <- cond.means.data[[interaction.vars[2]]]
				# Which of the interactions is the numeric, which the categorical?
				if (class(value1) == "numeric") {
					cont.val <- value1
					cat.val <- value2
					cat.levels <- levels(data[[interaction.vars[2]]])
				} else {
					cont.val <- value2
					cat.val <- value1
					cat.levels <- levels(data[[interaction.vars[1]]])
				}
			
				row.vector <- get.row.vector.cat.cont(cat.levels, cat.val, cont.val)
				mod.matrix <- c()
				for (i in 1:nlevels) {
					mod.matrix <- rbind(mod.matrix,row.vector)
				}
			}
			
			### Make column labels
			intVar1 <- interaction.vars[1]
			intVar2 <- interaction.vars[2]
			if (intVar1 %in% mods[["numeric"]]) {
				cont.var <- intVar1
				cat.var <- intVar2
			} else {
				cont.var <- intVar2
				cat.var <- intVar1
			}
			cat.levels <- levels(data[[cat.var]])
			# Continuous label comes first, followed by cat levels
            colnames <- c(colnames, paste(cont.var,":",cat.levels[2:length(cat.levels)],sep=""))
			### END of make column labels
		} else {
			# two continuous variables # 1 column
	        value1 <- cond.means.data[[interaction.vars[1]]]
	        value2 <- cond.means.data[[interaction.vars[2]]]
			mod.matrix <- rep(value1*value2, nlevels)
			
			colnames <- c(colnames, paste(interaction.vars[1],":",interaction.vars[2], sep=""))
		}
		overall.mod.matrix <- cbind(overall.mod.matrix,mod.matrix)
		
	} # end for interactions
	dsn.matrix <- cbind(dsn.matrix, overall.mod.matrix)
	# Set helpful dimnames
	dimnames(dsn.matrix) <- list(rownames, colnames)
	return(dsn.matrix)
}

#mod.matrix.for.strat.cov.in.cat.cat <- function()
get.row.vector.cat.cat <- function(cat1.levels, cat2.levels, value1, value2) {	
	# Returns a row vector for part of the mod.matrix for a cat:cat interaction
	# given the levels of the categories and the values the categories take
	row.vector <- c()
	# We need to generate a vector, then replicate it 
	# iterate over column values
	# Note: we ignore the first level in each category since it is naturally
	# coded
	for (y in cat2.levels[2:length(cat2.levels)]) {
		# rma varies 1st covariate faster than 2nd
		for (x in cat1.levels[2:length(cat1.levels)]) {
			row.vector <- c(row.vector, ifelse(y==value2 && x==value1, 1,0))
		}
	}
	
	return(row.vector)
}

get.row.vector.cat.cont <- function(cat.levels, cat.val, cont.val) {
	# cat.levels: levels of categorical covariate
	# cat.val: chosen value of categorical variable (a level)
	# cont.val: chosen value of continuous variable
	
	row <- c()
	# ignore first level since it is naturally coded
	for (x in cat.levels[2:length(cat.levels)]) {
		row <- c(row, ifelse(x==cat.val, 1, 0))
	}
	row <- cont.val * row
	return(row)
}


coded.cat.mod.level <- function(lvl, l.mod) {
	# gives a coded representation of the moderator according to the order
	# of levels in l.mod
	# l.mod: levels in the moderator
	# lvl chosen lvl to get the coding for
	#
	# E.g. if levels(moderator) == c("USA","CANADA","CHINA")
	# lvl
	# "USA" --> c(0,0)
	# "CANADA" --> c(1,0)
	# "CHINA" -- > c(0,1)
	
	# Find index of lvl in l.mod
	index <- match(lvl, l.mod)
	n.levels <- length(l.mod)
	
	# Make coding matrix e.g.
	# USA    0 0 # zero vector
	# CANADA 1 0 # identity matrix 
	# CHINA  0 1 # 
	code.matrix <- rbind(rep(0,n.levels-1),diag(n.levels-1))
	code.matrix[index,]
}



g.meta.regression <- function(data, mods, method, level, digits, btt=NULL) {
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
	
	mods.str <- make.mods.str(mods)
	
	# obtain regression result rma.uni
	res <- regression.wrapper(data, mods.str, method, level, digits,btt)

	results <- list(#"images"=images,
			"Summary"=paste(capture.output(res), collapse="\n"), # convert print output to a string
			#"plot_names"=plot.names,
			#"plot_params_paths"=plot.params.paths,
			"res"=res,
			"res.info"=rma.uni.value.info())
}

g.meta.regression.cond.means <- function(data, mods, method, level, digits, strat.cov, cond.means.data) {
	# Same as g.meta.regression. except we have conditional means output
	# strat_cov: the categorical covariate (name) to stratify the results of the conditional means over
	# cond.means.data: The values for the other covariates given as a list:
	#     List(cov1_name=cov1_val, cov2_cat_name=cov2_level,...)
	
	mods.str <- make.mods.str(mods)
	
	# obtain regression result rma.uni
	res <- regression.wrapper(data, mods.str, method, level, digits,btt=NULL)
	
	### Generate conditional means output
	A <- make.design.matrix(strat.cov, mods, cond.means.data, data)
	new_betas <- A %*% res$b
	new_cov   <- A %*% res$vb %*% t(A)
	new_vars <- diag(new_cov)
	alpha <- 1.0-(level/100.0)
	mult <- abs(qnorm(alpha/2.0))
	new_lowers <- new_betas - mult*sqrt(new_vars)
	new_uppers <- new_betas + mult*sqrt(new_vars)
	new_se     <- sqrt(new_vars)
	
	cond.means.df <- data.frame(cond.mean=new_betas, se=new_se, var=new_vars, ci.lb=new_lowers, ci.ub=new_uppers)
	cond.means.df <- round(cond.means.df, digits=digits)
	### END of conditional means output generation
	
	results<-list(
			      "Summary"=paste(capture.output(res), collapse="\n"),
				  "res"=res,
				  "res.info"=rma.uni.value.info(),
				  "Conditional Means Summary"=paste(capture.output(cond.means.df), collapse="\n"),
				  "res.cond.means"=cond.means.df
				)
}


meta.regression <- function(reg.data, params, cond.means.data=NULL, stop.at.rma=FALSE) {
	cov.data <- extract.cov.data(reg.data)
	cov.array <- cov.data$cov.array
	cat.ref.var.and.levels <- cov.data$cat.ref.var.and.levels

	# remove when and if method dialog is added
	method <- as.character(params$rm.method)
   

	
	res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
					level=params$conf.level, digits=params$digits,
					method=method, mods=cov.array)
	pure.res<-res
	# Used for when we just need the intermediate results (e.g. bootstrapping)
	if (stop.at.rma) {
		return(res) 
	}	
				
#   if (class(res)[1] != "try-error") {
       display.data <- cov.data$display.data
       reg.disp <- create.regression.display(res, params, display.data)
   
	   # 1 continuous covariate, no categorical covariates
       if (display.data$n.cont.covs==1 & length(display.data$factor.n.levels)==0) {
            # if only 1 continuous covariate, create reg. plot
            betas <- res$b
            fitted.line <- list(intercept=betas[1], slope=betas[2])
            plot.path <- "./r_tmp/reg.png"
            plot.data <- create.plot.data.reg(reg.data, params, fitted.line)

            # @TODO x and y labels ought to be passed in, probably
            plot.data$xlabel <- reg.data@covariates[[1]]@cov.name
            scale.str <- get.scale(params)
            if ((scale.str=="standard") || (scale.str=="arcsine")) {
                scale.str <- ""
                # This is for the y-axis label on regression plot - don't add "standard" or "arcsine" to label.
            }
            plot.data$ylabel <- paste(scale.str, " ", pretty.metric.name(as.character(params$measure)), sep="")
            meta.regression.plot(plot.data, plot.path)
            
            # write the plot data to disk so we can save it
            # @TODO will want to write the params data, too,
            # eventually
            plot.data.path <- save.plot.data(plot.data)

            images <- c("Regression Plot"=plot.path)
            plot.names <- c("reg.plot"="reg.plot")
            reg.plot.params.path <- save.plot.data(plot.data)
            plot.params.paths <- c("Regression Plot"=plot.data.path)
			pure.res$weights <- weights(res)
            results <- list("images"=images,
					        "Summary"=reg.disp,
							"plot_names"=plot.names,
                            "plot_params_paths"=plot.params.paths,
							"res"=pure.res,
							"res.info"=rma.uni.value.info())
		} else if (isnt.null(cond.means.data)) { # Give the conditional means results
			mr.cond.means.disp <- cond_means_display(res, params, display.data, reg.data=reg.data, cat.ref.var.and.levels=cat.ref.var.and.levels, cond.means.data=cond.means.data)
			res.output <- c(pure.res,
							list(Conditional_Means_Section=paste("############################",cond.means.info(cond.means.data), sep="\n"),
								 Conditional_Means=mr.cond.means.disp))
			res.output.info <- c(rma.uni.value.info(),
								 list(Conditional_Means_Section = list(type="vector", description=""),
						              Conditional_Means=list(type="blob", description="")))
			results <- list("Summary"=reg.disp,
							"Conditional Means"=mr.cond.means.disp,
							"res"= res.output,
							"res.info"= res.output.info
							  )
							
							
		} else if (display.data$n.cont.covs==0 & length(display.data$factor.n.levels)==1) {
			adj.reg.disp <- adjusted_means_display(res, params, display.data)
			res.output <- c(pure.res,
							list(Adjusted_Means_Section="#############################",
								 Adjusted_Means=adj.reg.disp))
			res.output.info <- c(rma.uni.value.info(),
								 list(Adjusted_Means_Section=list(type="vector", description=""),
									  Adjusted_Means=list(type="blob", description="")))
			results <- list("Summary"=reg.disp,
                            "Adjusted Mean"=adj.reg.disp,
							"res"=res.output,
							"res.info"=res.output.info)
		} else {
			results <- list("Summary"=reg.disp,
							"res"=pure.res,
							"res.info"=rma.uni.value.info())
		}
	
	references <- "Meta Regression: meta regression citation placeholder"
	results[["References"]] <- references
    results
}

cond.means.info <- function(cond.means.data) {
	blurb <- paste("\nConditional means for '",as.character(cond.means.data$chosen.cov.name), "',\nstratified over its levels given the following values for the other covariates:\n", sep="")
	for (name in names(cond.means.data)) {
		if (name != 'chosen.cov.name') {
			blurb <- paste(blurb, name, " = ", cond.means.data[[name]], "\n", sep="")
		}
	}
	return(blurb)
}


extract.cov.data <- function(reg.data, dont.make.array = FALSE) {
  # separate continuous and factor covariates and extract data.
  # The following are passed to create.regression.display
  n.cont.covs <- 0
  factor.n.levels <- NULL # vector containing number of levels for each factor covariate
  factor.cov.display.col <- NULL
  levels.display.col <- NULL
  studies.display.col <- NULL
  
  # initialize names of continuous covariates to empty list
  cont.cov.names <- c()
  cont.cov.array <- NULL
  factor.cov.array <- NULL
  cat.cov.ref.var.and.levels <- list() #### 
  for (n.covs in 1:length(reg.data@covariates)) {
    # put covariate data into two arrays, for continuous and factor covariates.
    cov <- reg.data@covariates[[n.covs]]
    cov.name <- cov@cov.name
    cov.vals <- cov@cov.vals
    cov.type <- cov@cov.type
	#debug_print <- paste(c("Cov name: ", cov.name, "\nCov type: ", cov.type,"\n"))
	#print(debug_print)
    ref.var <- cov@ref.var
    if (cov.type=="continuous") {
      cov.col <- array(cov.vals, dim=c(length(reg.data@y), 1), 
                    dimnames=list(NULL, cov.name))
      cont.cov.array <- cbind(cont.cov.array, cov.col)
      cont.cov.names <- c(cont.cov.names, cov.name)
      n.cont.covs <- n.cont.covs + 1
    }
    #factor.cov.array <- NULL   # was this causing issue # 222 ?
    if (cov.type=="factor") {
      levels <- sort(unique(cov.vals)) # it is actually important for this to be sorted 
      # Remove "" from levels, if necessary.
      levels.minus.NA <- setdiff(levels, "")
      # Levels except for reference variable
      levels.minus.ref.var <- setdiff(levels.minus.NA, ref.var)
	  
	  
      cov.cols <- array(dim=c(length(reg.data@y), length(levels.minus.ref.var)))
      studies.col <- c(sum(cov.vals==ref.var))
      for (col.index in 1:length(levels.minus.ref.var)) {
           level <- levels.minus.ref.var[col.index]
		   if (!dont.make.array) {
               cov.cols[cov.vals!="" & cov.vals!=level, col.index] <- 0
               cov.cols[cov.vals!="" & cov.vals==level, col.index] <- 1
	       }
           studies.col <- c(studies.col, sum(cov.vals==level)) 
      }
      factor.cov.array <- cbind(factor.cov.array, cov.cols)
      factor.n.levels <- c(factor.n.levels, length(levels.minus.NA))
      factor.cov.display.col <- c(factor.cov.display.col, cov.name, rep("",length(levels.minus.ref.var)))
      factor.studies.display.col <- c() 
      levels.display.col <- c(levels.display.col, ref.var, levels.minus.ref.var)
      studies.display.col <- c(studies.display.col, studies.col)
	  ref.var.and.levels.in.order <- c(ref.var, levels.minus.ref.var) ####
	  cat.cov.ref.var.and.levels[[cov.name]] <- ref.var.and.levels.in.order ####
      }
  }
  cov.array <- cbind(cont.cov.array, factor.cov.array)
  cov.display.col <- c("Intercept", cont.cov.names, factor.cov.display.col)
  levels.display.col <- c(rep("",length(cont.cov.names) + 1), levels.display.col)
  studies.display.col <- c(rep("",length(cont.cov.names) + 1), studies.display.col)
  display.data <- list(cov.display.col=cov.display.col, levels.display.col=levels.display.col,
                       studies.display.col=studies.display.col, factor.n.levels=factor.n.levels, n.cont.covs=n.cont.covs)
  
  cov.data <- list(cov.array=cov.array, display.data=display.data, cat.ref.var.and.levels=cat.cov.ref.var.and.levels)
                   
}

binary.fixed.meta.regression <- function(reg.data, params){
  # meta regression for numerical covariates
    cov.data <- array(dim=c(length(reg.data@y), length(cov.names)), dimnames=list(NULL, cov.names))  
    for (cov.name in cov.names) {
      # extract matrix of covariates
       cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
       cov.vals <- eval(parse(text=cov.val.str))
       cov.data[,cov.name] <- cov.vals
    }     
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.data)
    reg.disp <- create.regression.disp(res, params, cov.names)
    if (length(cov.names)==1) {
        # if just 1 covariate, create reg. plot
        betas <- res$b
        fitted.line <- list(intercept=betas[1], slope=betas[2])
        plot.path <- "./r_tmp/reg.png"
        plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
        meta.regression.plot(plot.data, outpath=plot.path, symSize=1,
                                  lcol = "darkred",
                                  y.axis.label = "Effect size",
                                  xlabel= cov.name,
                                  lweight = 3,
                                  lpatern = "dotted",
                                  plotregion = "n",
                                  mcolor = "darkgreen",
                                  regline = TRUE)   
        images <- c("Regression Plot"=plot.path)
        plot.names <- c("forest plot"="reg.plot")
        results <- list("images"=images, "Summary"=reg.disp, "plot_names"=plot.names)
    } else {
        results <- list("Summary"=reg.disp)
    }

}

random.meta.regression <- function(reg.data, params, cov.name){
    cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
    cov.vals <- eval(parse(text=cov.val.str))
    res<-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, 
                                method=params$rm.method, 
                                mods=cov.vals)
    reg.disp <- create.regression.disp(res, params)
    reg.disp
    betas <- res$b
    fitted.line <- list(intercept=betas[1], slope=betas[2])
    # temporary fix until params$rp_outpath is added to the GUI
    if (is.null(params$rp_outpath)) {
        plot.path <- "./r_tmp/reg.png"
    }
    else {
        plot.path <- params$rp_outpath
    }
    plot.data <- create.plot.data.reg(reg.data, params, fitted.line, selected.cov=cov.name)
    meta.regression.plot(plot.data, outpath=plot.path, symSize=1,
                                  lcol = "darkred",
                                  y.axis.label = "Effect size",
                                  xlabel= cov.name,
                                  lweight = 3,
                                  lpatern = "solid",
                                  plotregion = "n",
                                  mcolor = "black",
                                  regline = TRUE)   
    images <- c("Regression Plot"=plot.path)
    plot.names <- c("forest plot"="reg.plot")
    results <- list("images"=images, "Summary"=reg.disp, "plot_names"=plot.names)
    results
}

binary.random.meta.regression.parameters <- function(){
    # parameters
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    params <- list("rm.method"=rm_method_ls, "conf.level"="float", "digits"="float")
    
    # default values
    defaults <- list("rm.method"="DL", "conf.level"=95, "digits"=3)
    
    var_order <- c("rm.method", "conf.level", "digits")
    parameters <- list("parameters"=params, "defaults"=defaults, "var_order"=var_order)
}

categorical.meta.regression <- function(reg.data, params, cov.names) {
  # meta-regression for categorical covariates 
  cov.data <- array()
  var.names <- NULL
  for (cov.name in cov.names) {
      # extract matrix of covariates
       cov.val.str <- paste("reg.data@covariates$", cov.name, sep="")
       groups <- eval(parse(text=cov.val.str))
       group.list <- unique(groups)
       array.tmp <- array(dim=c(length(reg.data@y), length(group.list)-1), dimnames=list(NULL, group.list[-1]))
       for (group in group.list[-1]) {
           array.tmp[,group] <- as.numeric(groups == group)
       }
       if (length(cov.data) > 1) {
           cov.data <- cbind(cov.data, array.tmp)
       } else {
           cov.data <- array.tmp
       }
  }
  res <-rma.uni(yi=reg.data@y, sei=reg.data@SE, slab=reg.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", 
                                mods=cov.data)
  reg.disp <- create.regression.disp(res, params, cov.names=dimnames(cov.data)[[2]]) 
  results <- list("Summary"=reg.disp)
}
