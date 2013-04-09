###############################################################################
# global value set from python to control confidence level. At the moment, it
# only affects calc.box.sizes in plotting.R

get.mult.from.conf.level <- function() {
	alpha <- 1.0-(CONF.LEVEL.GLOBAL/100.0)
	mult <- abs(qnorm(alpha/2.0))
}

set.global.conf.level <- function(conf.level) {
	CONF.LEVEL.GLOBAL <<- conf.level
	cat("R: Confidence level is now", CONF.LEVEL.GLOBAL)
	return(CONF.LEVEL.GLOBAL)
}

get.global.conf.level <- function() {
	return(CONF.LEVEL.GLOBAL)
}
################################################################################
	
