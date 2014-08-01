############################################################################
# permuted.ma
#
# Description: Performs a permuted (a.k.a. randomized even though it's not 
# 	random) meta analysis 
#
# Meta-analysis parameters:
#	data: data frame of the data with columns:
#		yi: effect size
#		vi: variance
#		slab: study labels 
#	method: character string specifying whether a fixed- or a
#		random/mixed-effects model should be fitted.
#		Fixed-effects model: "FE"
#		Random-effects model one of: "DL", "HE", "SJ", "ML",
#			"REML", "EB", or "HS"
#	level: confidence interval level (0-100)
#	digits: # of decimal places to which the printed results should be rounded.
#	btt:
#	knha: 
#
# Permuation parameters:
#	exact:
#	iter:
#	retpermdist:
#	makepermdisthist:
#
############################################################################
permuted.ma <- function(
	# meta-analysis parameters
	data, mods, method, intercept=TRUE, level=95, digits=4, knha=FALSE,
	# Permutation parameters
	exact=FALSE, iter=1000, retpermdist=FALSE) {
	
	ma.res <- rma.uni(yi, vi,
		intercept=intercept,
		data=data,
		slab=data$slab, 
		method=method,
		knha=knha,
		level=level,
		digits=digits)

	perm.res <- permutest(ma.res, exact=exact, iter=iter,
              retpermdist=FALSE, digits=digts)
	summary <- paste(capture.output(res), collapse="\n")

	results <- list(
		"Summary"=Summary)
}

permuted.meta.reg <- function (
	# meta-analysis parameters
	data, method, intercept=TRUE, level=95, digits=4, knha=FALSE, btt=NULL,
	# Permutation parameters
	exact=FALSE, iter=1000, retpermdist=FALSE) {

	mods.str <- make.mods.str(mods)
	
	# obtain regression result rma.uni
	reg.res <- regression.wrapper(data, mods.str, method, level, digits, btt)

	perm.res <- permutest(ma.res, exact=exact, iter=iter,
              retpermdist=FALSE, digits=digts)
	summary <- paste(capture.output(res), collapse="\n")

	results <- list(
		"Summary"=Summary)
}











