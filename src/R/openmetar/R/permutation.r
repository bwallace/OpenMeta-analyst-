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
	data, method, intercept=TRUE, level=95, digits=4, knha=FALSE, weighted=TRUE,
	# Permutation parameters
	exact=FALSE, iter=1000, retpermdist=FALSE) {
	
	ma.res <- rma.uni(yi, vi,
		intercept=intercept,
		data=data,
		slab=data$slab, 
		method=method,
		knha=knha,
		level=level,
		digits=digits,
		weighted=weighted)

	perm.res <- permutest(ma.res, exact=exact, iter=iter,
              retpermdist=FALSE, digits=digits)
	summary <- paste(capture.output(perm.res), collapse="\n")

	results <- list(
		"Summary"=summary,
		"res"=perm.res,
		"res.info"=permutest.value.info(retpermdist, meta.reg.mode=FALSE)
		)
}

permuted.meta.reg <- function (
	# meta-regresion parameters
	data, method, mods, intercept=TRUE, level=95, digits=4, knha=FALSE, btt=NULL,
	# Permutation parameters
	exact=FALSE, iter=1000, retpermdist=FALSE,
	# Other parameters
	include.meta.reg.summary=TRUE # show regular meta regression results too in output
	) {

	mods.str <- make.mods.str(mods)
	
	# obtain regression result rma.uni
	reg.res <- regression.wrapper(data, mods.str, method, level, digits, btt)

	perm.res <- permutest(reg.res, exact=exact, iter=iter,
              retpermdist=retpermdist, digits=digits)
	summary <- paste(capture.output(perm.res), collapse="\n")

	results <- list(
		"Permuted Meta-Regression Summary"=summary,
		"res"=perm.res,
		"res.info"=permutest.value.info(retpermdist)
		)

	if (include.meta.reg.summary) {
		meta.reg.result <- g.meta.regression(
			data=data,
			mods=mods,
			method=method,
			level=level,
			digits=digits,
			measure=NULL,
			btt=btt,
			make.coeff.forest.plot=FALSE,
			exclude.intercept=FALSE, # For coefficient forest plot
			disable.plots=TRUE
		)

		results <- c(list('Standard Meta Regression Summary'=meta.reg.result$Summary), results)
	}

	results
}

permutest.value.info <- function(retpermdist, meta.reg.mode=TRUE) {
	info = list(
			pval = list(type="vector", description='p-value(s) based on the permutation test.'),
			QMp = list(type="vector", description='p-value for the omnibus test of coefficients based on the permutation test.')
	)
	
	if (retpermdist && meta.reg.mode) {
		additional.info = list(
				zval.perm = list(type="data.frame", description='values of the test statistics of the coefficients under the various permutations'),
				QM.perm = list(type="vector", description='values of the test statistic for the omnibus test of coefficients under the various permutations')
		)
		info = c(info, additional.info)
	}
	return(info)
}

