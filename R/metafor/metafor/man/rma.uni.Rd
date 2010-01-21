\name{rma.uni}
\alias{rma.uni}
\alias{rma}
\title{Meta-Analysis via the General Linear (Mixed-Effects) Model}
\description{Function to fit the meta-analytic fixed- and random-effects models with or without moderators via the general linear (mixed-effects) model. See the documentation of the \pkg{\link{metafor-package}} for more details on these models.}
\usage{
rma.uni(yi, vi, sei, ai, bi, ci, di, n1i, n2i, m1i, m2i, sd1i, sd2i, 
        xi, mi, ri, ni, mods=NULL, data=NULL, intercept=TRUE, slab=NULL, 
        subset=NULL, measure="GEN", add=1/2, to="only0", vtype="LS", 
        method="REML", weighted=TRUE, level=95, digits=4, btt=NULL, 
        tau2=NULL, knha=FALSE, control=list())
rma(yi, vi, sei, ai, bi, ci, di, n1i, n2i, m1i, m2i, sd1i, sd2i, 
        xi, mi, ri, ni, mods=NULL, data=NULL, intercept=TRUE, slab=NULL, 
        subset=NULL, measure="GEN", add=1/2, to="only0", vtype="LS", 
        method="REML", weighted=TRUE, level=95, digits=4, btt=NULL, 
        tau2=NULL, knha=FALSE, control=list())
}
\arguments{
	\item{yi}{vector of length \eqn{k} with the observed effect sizes or outcomes. See \sQuote{Details}.}
	\item{vi}{vector of length \eqn{k} with the corresponding sampling variances. See \sQuote{Details}.}
	\item{sei}{vector of length \eqn{k} with the corresponding standard errors. See \sQuote{Details}.}
	\item{ai}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{bi}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{ci}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{di}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{n1i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{n2i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{m1i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{m2i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{sd1i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{sd2i}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{xi}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{mi}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{ri}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{ni}{see below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{mods}{an optional argument to include one or more moderators in the model. A single moderator can be given as a vector of length \eqn{k} specifying the values of the moderator. Multiple moderators are specified by giving a matrix with \eqn{k} rows and \eqn{p'} columns.}
	\item{data}{an optional data frame containing the variables given to the arguments above.}
	\item{intercept}{logical, indicating whether an intercept term should be added to the model (default is \code{TRUE}).}
	\item{slab}{an optional vector with unique labels for the \eqn{k} studies.}
	\item{subset}{an optional vector indicating the subset of studies that should be used for the analysis. This can be a logical vector of length \eqn{k} or a numeric vector indicating the indices of the observations to include.}
	\item{measure}{a character string indicating the type of data supplied to the function. When \code{measure="GEN"} (default), the observed effect sizes or outcomes and corresponding sampling variances (or standard errors) should be supplied to the function via the \code{yi}, \code{vi}, and \code{sei} arguments (only one of the two, \code{vi} or \code{sei}, needs to be specified). Alternatively, one can set \code{measure} to one of the effect size or outcome measures described under the documentation for the \code{\link{escalc}} function and supply the needed data via the appropriate arguments.}
	\item{add}{see the documentation of the \code{\link{escalc}} function.}
	\item{to}{see the documentation of the \code{\link{escalc}} function.}
	\item{vtype}{see the documentation of the \code{\link{escalc}} function.}
	\item{method}{a character string specifying whether a fixed- or a random/mixed-effects model should be fitted. A fixed-effects model (with or without moderators) is fitted when using \code{method="FE"}. Random/mixed-effects models are fitted by setting \code{method} equal to one of the following: \code{"HE"}, \code{"DL"}, \code{"SJ"}, \code{"ML"}, \code{"REML"}, or \code{"EB"}. Default is \code{"REML"}. See \sQuote{Details}.}
	\item{weighted}{logical indicating whether weighted (default) or unweighted least squares should be used to fit the model.}
	\item{level}{a numerical value between 0 and 100 specifying the confidence interval level (default is 95).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (default is 4).}
	\item{btt}{an optional vector of indices specifying which coefficients to include in the omnibus test of moderators. See \sQuote{Details}.}
	\item{tau2}{an optional numerical value to specify the amount of (residual) heterogeneity in a random- or mixed-effects model (instead of estimating it). Useful for sensitivity analyses (e.g., for plotting results as a function of \eqn{\tau^2}{tau^2}). The default is \code{NULL}, which means that \eqn{\tau^2}{tau^2} is estimated from the data.}
	\item{knha}{logical specifying whether the method by Knapp and Hartung (2003) should be used for adjusting test statistics and confidence intervals (default is \code{FALSE}). See \sQuote{Details}.}
	\item{control}{optional list of control values for the iterative estimation algorithms. Defaults to an empty list, which means that default values are defined inside the function. See \sQuote{Note}.}
}
\details{

	\bold{Specifying the Data}

	The function can be used in conjunction with any of the usual effect size or outcome measures used in meta-analyses (e.g., log odds ratios, log risk ratios, risk differences, mean differences, standardized mean differences, raw correlation coefficients, correlation coefficients transformed with Fisher's r-to-z transformation, and so on). Simply supply the observed outcomes via the \code{yi} argument and the corresponding sampling variances via the \code{vi} argument (or supply the standard errors, the square root of sampling variances, via the \code{sei} argument). In this case, one should set \code{measure="GEN"} (the default).

	Alternatively, the function can automatically calculate many effect size or outcome measures (and the corresponding sampling variances) when supplied with the needed data. The \code{\link{escalc}} function describes which measures are currently implemented and what data/arguments should then be specified. The \code{measure} argument should then be set to the desired measure.

	\bold{Specifying the Model}

	Assuming the observed outcomes and corresponding sampling variances are supplied via \code{yi} and \code{vi}, the \bold{fixed-effects model} is fitted with \code{rma(yi, vi, method="FE")}. The \bold{random-effects model} is fitted with the same code but setting \code{method} to one of the various estimators for the amount of heterogeneity:
	\itemize{
	\item \code{method="HS"} = Hunter-Schmidt estimator
	\item \code{method="HE"} = Hedges estimator
	\item \code{method="DL"} = DerSimonian-Laird estimator
	\item \code{method="SJ"} = Sidik-Jonkman estimator
	\item \code{method="ML"} = maximum-likelihood estimator
	\item \code{method="REML"} = restricted maximum-likelihood estimator
	\item \code{method="EB"} = empirical Bayes estimator.
	}
	One or more moderators can be included in these models via the \code{mods} argument. A single moderator can be given as a (row or column) vector of length \eqn{k} specifying the values of the moderator. Multiple moderators are specified by giving a matrix with \eqn{k} rows and \eqn{p'} columns (e.g., using \code{mods = cbind(mod1, mod2, mod3)}, where \code{mod1}, \code{mod2}, \code{mod3} correspond to the names of the variables for the three moderator variables). A \bold{fixed-effects with moderators model} is then fitted by setting \code{method="FE"}, while a \bold{mixed-effects model} is fitted by specifying one of the estimators for the amount of (residual) heterogeneity given earlier. The intercept is automatically included in the model, unless \code{intercept=FALSE} is used.

	\bold{Omnibus Test of Parameters}

	In models with more than one independent variable, an omnibus test of all the regression coefficients is conducted that excludes the intercept (the first coefficient) if the option \code{intercept=TRUE} is used (which is the default). If \code{intercept=FALSE}, then the omnibus test includes all of the coefficients in the model. Alternatively, one can specify the indices of the coefficients to test via the \code{btt} argument. For example, use \code{btt=c(3,4)} to only include the third and fourth coefficient from the model in the test.

	\bold{Categorical Moderators}

	Categorical moderator variables can be included in the model in the same way that appropriately (dummy) coded categorical independent variables can be included in linear models. You have to do the dummy coding yourself or use the \code{\link{model.matrix}} function to do the coding for you. An example is shown below.

	\bold{Knapp & Hartung Adjustment}

	By default, the test statistics of the individual coefficients in the model (and the corresponding confidence intervals) are based on the normal distribution, while the omnibus test is based on a chi-square distribution with \eqn{m} degrees of freedom (\eqn{m} being the number of coefficients tested). The Knapp and Hartung (2003) method (\code{knha=TRUE}) is an adjustment to the standard errors of the estimated coefficients, which helps to account for the uncertainty in the estimate of the amount of (residual) heterogeneity and leads to different reference distributions. Individual coefficients and confidence intervals are then based on the t-distribution with \eqn{k-p} degrees of freedom (\eqn{p} being the total number of coefficients in the model), while the omnibus test statistic then uses an F-distribution with \eqn{m} and \eqn{k-p} degrees of freedom. The Knapp and Hartung (2003) method is only meant to be used in the context of random- or mixed-effects models.
}
\value{
	An object of class \code{c("rma.uni", "rma")}. The object is a list containing the following components:
	\item{b}{estimated coefficients of the model.}
	\item{se}{standard errors of the coefficients.}
	\item{zval}{test statistics of the coefficients.}
	\item{pval}{p-values for the test statistics.}
	\item{ci.lb}{lower bound of the confidence intervals for the coefficients.}
	\item{ci.ub}{upper bound of the confidence intervals for the coefficients.}
	\item{vb}{variance-covariance matrix of the estimated coefficients.}
	\item{tau2}{estimated amount of (residual) heterogeneity. Always \code{0} when \code{method="FE"}.}
	\item{se.tau2}{estimated standard error of the estimated amount of (residual) heterogeneity when using ML or REML estimation (\code{NA} otherwise).}
	\item{k}{number of outcomes included in the model fitting (equal to \code{length(yi)} unless \code{subset} was used or if there are missing data).}
	\item{p}{number of coefficients in the model (including the intercept).}
	\item{m}{number of coefficients included in the omnibus test of coefficients.}
	\item{QE}{test statistic for the test of (residual) heterogeneity.}
	\item{QEp}{p-value for the test of (residual) heterogeneity.}
	\item{QM}{test statistic for the omnibus test of coefficients.}
	\item{QMp}{p-value for the omnibus test of coefficients.}
	\item{I2}{value of \eqn{I^2} (only for the random-effects model; \code{NA} otherwise).}
	\item{H2}{value of \eqn{H^2} (only for the random-effects model; \code{NA} otherwise).}
	\item{int.only}{logical that indicates whether the model only includes an intercept.}
	\item{yi, vi, X}{the vector of outcomes, the corresponding sampling variances, and the design matrix of the model.}
	\item{fit.stats}{a list with the log likelihood, deviance, AIC, and BIC values under the unrestricted and restricted likelihood.}
	\item{\dots}{some additional elements/values.}

	The results of the fitted model are neatly formated and printed with the \code{\link{print.rma.uni}} function. If you also want the fit statistics, use \code{\link{summary.rma}} (or use the \code{\link{fitstats.rma}} function to extract them). Full versus reduced model comparisons in terms of fit statistics and likelihoods can be obtained with \code{\link{anova.rma.uni}}. Permutation tests for the model coefficient(s) can be obtained with \code{\link{permutest.rma.uni}}.

	Predicted/fitted values can be obtained with \code{\link{predict.rma.uni}} and \code{\link{fitted.rma}}. For best linear unbiased predictions, see \code{\link{blup.rma.uni}}.

	The \code{\link{residuals.rma}}, \code{\link{rstandard.rma.uni}}, and \code{\link{rstudent.rma.uni}} functions extract raw and standardized residuals. Additional case diagnostics (e.g., to determine influential studies) can be obtained with the \code{\link{influence.rma.uni}} function. For models without moderators, leave-one-out diagnostics can also be obtained with \code{\link{leave1out.rma.uni}}.

	A confidence interval for the amount of (residual) heterogeneity in the random/mixed-effects model can be obtained with \code{\link{cint.rma.uni}}.

	Forest, funnel, and radial plots (the latter only for models without moderators) are drawn with \code{\link{forest.rma}}, \code{\link{funnel.rma}}, and \code{\link{radial.rma}}. The \code{\link{qqnorm.rma.uni}} function provides a normal QQ plot of the standardized residuals. One can also just call \code{\link{plot.rma.uni}} on the fitted model object to obtain various plots at once.

	Tests for publication bias (or more accurately, for funnel plot asymmetry) can be obtained with \code{\link{ranktest.rma}} and \code{\link{regtest.rma}}.

	For models without moderators, a cumulative meta-analysis (i.e., adding one obervation at a time) can be obtained with \code{\link{cumul.rma.uni}}.

	Other assessor functions include \code{\link{coef.rma}}, \code{\link{vcov.rma}}, \code{\link{logLik.rma}}, and \code{\link{hatvalues.rma.uni}}.
}
\note{
	The ML, REML, and EB estimators of \eqn{\tau^2}{tau^2} are obtained via the Fisher scoring algorithm, which is robust to poor starting values and usually converges quickly (Harville, 1977; Jennrich & Sampson, 1976). By default, the starting value is set equal to the value of the Hedges estimator and the algorithm terminates when the change in the estimated value of \eqn{\tau^2}{tau^2} is smaller than \eqn{10^{-5}}{10^(-5)} from one iteration to the next. The maximum number of iterations is 50 by default. A different starting value, threshold, and maximum number of iterations can be specified via the \code{control} argument by setting \code{control=list(tau2.init=value, threshold=value, maxiter=value)}. The step length of the Fisher scoring algorithm can also be manually adjusted by a desired factor with \code{control=list(stepadj=value)} (values below 1 will reduce the step length). Information on the evolution of the algorithm is obtained with \code{control=list(verbose=TRUE)}.

	All of the heterogeneity estimators except SJ can in principle yield negative estimates for the amount of (residual) heterogeneity. However, negative estimates of \eqn{\tau^2}{tau^2} are outside of the parameter space. For the HS, HE, and DL estimators, negative estimates are therefore truncated to zero. For ML, REML, and EB estimation, the Fisher scoring algorithm makes use of step halving to guarantee a non-negative estimate. For those brave enough to step into risky territory, there is the option to set the lower bound of \eqn{\tau^2}{tau^2} equal to some other value besides zero with \code{control=list(tau2.min=value)}.

	The Hunter-Schmidt estimator for the amount of heterogeneity is defined in Hunter and Schmidt (1990) only in the context of the random-effects model when analyzing correlation coefficients. A general version of this estimator for the random-effects model not specific to any particular outcome measure is described in Viechtbauer (2005). The same idea can be easily extended to the mixed-effects model and is implemented here.

	Outcomes with non-positive sampling variances are problematic. If a sampling variance is equal to zero, then its weight will be \eqn{1/0} for fixed-effects models when using weighted estimation. Switching to unweighted estimation is a possible solution then. For random/mixed-effects model, some estimators of \eqn{\tau^2}{tau^2} are undefined when there is at least one sampling variance equal to zero. Other estimators may work, but it may still be necessary to switch to unweighted model fitting, especially when the estimate of \eqn{\tau^2}{tau^2} turns out to be zero.

	If you get a \sQuote{singular matrix} error when using the function, then this means that there is a linear relationship between the moderator variables included in the model. For example, two moderators that correlated perfectly would cause this error. Deleting (redundant) moderator variables from the model as needed should solve this problem.

	Finally, some general words of caution about the assumptions underlying the models are warranted:
	\itemize{
	\item The sampling variances (i.e., the \code{vi} values) are treated as if they were known constants. This (usually) implies that the distributions of the test statistics and corresponding confidence intervals are only exact and have nominal coverage when the within-study sample sizes are large (i.e., when the error in the sampling variance estimates is small). Certain outcome measures (e.g., the arc-sine transformed risk difference and Fisher's r-to-z transformed correlation coefficient) are based on variance stabilizing transformations that also help to make the assumption of known sampling variances much more reasonable.
	\item When fitting a mixed/random-effects model, \eqn{\tau^2}{tau^2} is estimated and then treated as a known constant thereafter. This ignores the uncertainty in the estimate of \eqn{\tau^2}{tau^2}. As a consequence, the standard errors of the parameter estimates tend to be too small, yielding test statistics that are too large and confidence intervals that are not wide enough. The Knapp and Hartung (2003) method can be used to counter this problem, yielding test statistics and confidence intervals whose properties are closer to nominal.
	\item Most effect size measures are not exactly normally distributed as assumed under the various models. However, the normal approximation usually becomes more accurate for most effect size or outcome measures as the within-study sample sizes increase. Therefore, sufficiently large within-study sample sizes are (usually) needed to be certain that the tests and confidence intervals have nominal levels/coverage. Again, certain outcome measures (e.g., Fisher's r-to-z transformed correlation coefficient) may be preferable from this perspective as well.
	}
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	DerSimonian, R. & Laird, N. (1986) Meta-analysis in clinical trials. \emph{Controlled Clinical Trials}, \bold{7}, 177--188.

	Harville, D. A. (1977) Maximum likelihood approaches to variance component estimation and to related problems. \emph{Journal of the American Statistical Association}, \bold{72}, 320--338.
	
	Hedges, L. V. (1983) A random effects model for effect sizes. \emph{Psychological Bulletin}, \bold{93}, 388--395.

	Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis}. San Diego, CA: Academic Press.

	Hunter, J. E. & Schmidt, F. L. (1990). \emph{Methods of meta-analysis: Correcting error and bias in
	research findings}. Newbury Park, CA: Sage.

	Jennrich, R. I. & Sampson, P. F. (1976) Newton-Raphson and related algorithms for maximum likelihood variance component estimation. \emph{Technometrics}, \bold{18}, 11--17.

	Knapp, G. & Hartung, J. (2003) Improved tests for a random effects meta-regression with a single covariate. \emph{Statistics in Medicine}, \bold{22}, 2693--2710.

	Morris, C. N. (1983) Parametric Empirical Bayes inference: Theory and applications (with discussion). \emph{Journal of the American Statistical Association}, \bold{78}, 47--65.

	Raudenbush, S. W. (1994) Random effects models. In H. C. Cooper & L. V. Hedges (Eds.), \emph{The handbook of research synthesis} (pp. 301--321). New York: Russell Sage Foundation.

	Sidik, K. & Jonkman, J. N. (2005a) A note on variance estimation in random effects meta-regression. \emph{Journal of Biopharmaceutical Statistics}, \bold{15}, 823--838.

	Sidik, K. & Jonkman, J. N. (2005b) Simple heterogeneity variance estimation for meta-analysis. \emph{Journal of the Royal Statistical Society, Series C}, \bold{54}, 367--384.

	Viechtbauer, W. (2005) Bias and efficiency of meta-analytic variance estimators in the random-effects model. \emph{Journal of Educational and Behavioral Statistics}, \bold{30}, 261--293.
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### random-effects model
rma(yi, vi, data=dat, method="REML")

### mixed-effects model with two moderators (absolute latitude and publication year)
rma(yi, vi, mods=cbind(ablat, year), data=dat, method="REML")

### supplying the raw data directly to the function
rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, mods=cbind(ablat, year), 
    data=dat, measure="RR", method="REML")

### dummy coding of the allocation factor
alloc.random     <- ifelse(dat$alloc == "random",     1, 0)
alloc.alternate  <- ifelse(dat$alloc == "alternate",  1, 0)
alloc.systematic <- ifelse(dat$alloc == "systematic", 1, 0)

### test the allocation factor (in the presence of the other moderators)
### note: "alternate" is the reference level of the allocation factor
### note: the intercept is the first coefficient, so btt=c(2,3)
rma(yi, vi, mods=cbind(alloc.random, alloc.systematic, year, ablat), 
    data=dat, method="REML", btt=c(2,3))

### use model.matrix() to code the factor and set up the design matrix
### careful: X already includes the intercept, so need to use intercept=FALSE
X <- model.matrix(~ factor(alloc) + year + ablat, data=dat)
rma(yi, vi, mods=X, intercept=FALSE, data=dat, method="REML", btt=c(2,3))
}
\keyword{models}
