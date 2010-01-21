\name{metafor-package}
\alias{metafor-package}
\alias{metafor}
\docType{package}
\title{Metafor: A Meta-Analysis Package for R}
\description{
	The \pkg{metafor} package provides functions for conducting meta-analyses in \R. Currently, there are functions to fit the meta-analytic fixed- and random-effects model via the general linear (mixed-effects) model, the Mantel-Haenszel method, and Peto's method (the latter two only for fixed-effects models). Moderators (study-level covariates) can be included when using the general linear (mixed-effects) model approach, allowing the user to fit \sQuote{meta-regression} models. The package also provides various plot functions (for example, for forest, funnel, and radial plots) and functions for assessing the model fit and obtaining case diagnostics.
}
\section{The rma.uni Function}{
	The various meta-analytic models that are usually applied in practice are special cases of the general linear (mixed-effects) model. The \code{\link{rma.uni}} function (with alias \code{\link{rma}}) provides a general framework for fitting the various models. The function can be used in conjunction with any of the usual effect size or outcome measures used in meta-analyses (e.g., log odds ratios, log risk ratios, risk differences, mean differences, standardized mean differences, raw correlation coefficients, correlation coefficients transformed with Fisher's r-to-z transformation, and so on). For details on these effect size or outcome measures, please see the documentation of the \code{\link{escalc}} function. The notation and models underlying the \code{\link{rma.uni}} function are explained below.

	For a set of \eqn{i = 1, \ldots, k}{i = 1, ..., k} independent studies, let \eqn{y_i} denote the observed value of the effect size or outcome measure in the \eqn{i^{th}}{ith} study. Let \eqn{\theta_i}{theta_i} denote the corresponding (unknown) true effect or outcome in the \eqn{i^{th}}{ith} study, such that \eqn{y_i | \theta_i \sim N(\theta_i, v_i)}{y_i | theta_i ~ N(theta_i, v_i)}. In other words, the observed effects or outcomes are assumed to be unbiased and normally distributed estimates of the corresponding (unknown) true effects or outcomes with sampling variances equal to \eqn{v_i}. The \eqn{v_i} values are assumed to be known.

	The \bold{fixed-effects model} conditions on the true effects or outcomes and therefore provides a \emph{conditional inference} about the set of \eqn{k} studies included in the meta-analysis. This implies that the fitted model provides an estimate of \eqn{\sum_{i=1}^k w_i \theta_i / \sum_{i=1}^k w_i}{sum w_i theta_i / sum w_i}, that is, the \emph{weighted average} of the true effects in the set of \eqn{k} studies, with weights equal to \eqn{w_i = 1/v_i}. One can also employ an unweighted estimation method, which provides an estimate of the \emph{unweighted average} of the true effects in the set of \eqn{k} studies (i.e., an estimate of \eqn{1/k \sum_{i=1}^k \theta_i}{sum theta_i / k}).

	Moderators can be included in the fixed-effects model, yielding a \bold{fixed-effects with moderators model}. Again, since the model conditions on the set of \eqn{k} studies included in the meta-analysis, the regression coefficients from the fitted model estimate the weighted least-squares relationship between the true effects and the moderator variables within the set of \eqn{k} studies included in the meta-analysis (again using weights equal to \eqn{w_i = 1/v_i}). The (unweighted) least-squares relationship between the true effects and the moderator variables can be obtained when using the unweighted estimation method.

	The \bold{random-effects model} does not condition on the true effects. Instead, the \eqn{k} studies included in the meta-analysis are assumed to be a random selection from a hypothetical population of studies. One can envision this hypothetical population as an essentially infinite set of studies comprising all of the studies that have been conducted, that could have been conducted, or that may be conducted in the future. The true effects or outcomes in this population of studies are assumed to be normally distributed with \eqn{\mu}{mu} denoting the average effect and \eqn{\tau^2}{tau^2} denoting the variance of the true effects in the population (\eqn{\tau^2}{tau^2} is therefore often referred to as the \sQuote{amount of heterogeneity} in the population of studies). The fitted model provides an estimate of \eqn{\mu}{mu} and \eqn{\tau^2}{tau^2}. Consequently, the random-effects model provides an \emph{unconditional inference} about the average effect in the population of studies from which the \eqn{k} studies included in the meta-analysis are assumed to be a random selection.

	When including moderator variables in the random-effects model, we obtain what is typically called a \bold{mixed-effects model} in the meta-analytic literature. The coefficients from the fitted model then estimate the relationship between the average true effect or outcome in the population of studies and the moderator variables included in the model. The value of \eqn{\tau^2}{tau^2} in the mixed-effects model denotes the \sQuote{amount of residual heterogeneity} in the true effects or outcomes (i.e., the amount of variability in the true effects or outcomes over and beyond the heterogeneity that is introduced by the moderators).

	One can also choose between weighted and unweighted estimation in the context of the random- and mixed-effects model, although the parameters that are estimated remain the same regardless of the estimation method used (as opposed to the fixed-effects model case, where the parameter estimated is different for weighted and unweighted estimation).

	Contrary to what is often stated in the literature, it is important to realize that the fixed-effects model does \emph{not} assume that the true effects or outcomes are homogeneous (i.e., that \eqn{\theta_i}{theta_i} is equal to some common value \eqn{\theta}{theta} for all \eqn{k} studies). Therefore, the fixed-effects model may be used even when the true effects or outcomes are heterogeneous. However, when using a fixed-effects model, we can only draw conclusions about the average effect (weighted or unweighted) in the set of \eqn{k} studies included in the meta-analysis. On the other hand, the random-effects model provides an inference about the average effect in the entire population of studies from which the included studies are assumed to be a random selection. 

	Moreover, the fixed-effects with moderators model does \emph{not} assume that the heterogeneity in the true effects or outcomes is fully accounted for by the moderators included in the model. However, when using a fixed-effects with moderators model, we must again restrict our inferences to the set of \eqn{k} studies included in the meta-analysis. For more details on the distinction between fixed- and random-effects models, see Hedges and Vevea (1998) and Laird and Mosteller (1990).
}
\section{The rma.mh Function}{
	The Mantel-Haenszel method provides an alternative approach to fitting the fixed-effects model when dealing with studies providing data in the form of 2x2 tables (Mantel & Haenszel, 1959). The method is particularly advantageous when aggregating a large number of tables with small sample sizes (the so-called sparse data or increasing strata case). The Mantel-Haenszel method is implemented in the \code{\link{rma.mh}} function. It can be used in combination with odds ratios, risk ratios, and risk differences. The Mantel-Haenszel method is always based on a weighted estimation approach.
}
\section{The rma.peto Function}{
	Yet another method that can be used in the context of a meta-analysis of 2x2 tables is Peto's method (see Yusuf et al., 1985), implemented in the \code{\link{rma.peto}} function. It is a weighted estimation approach for the combination of odds ratios.
}
\section{Future Plans and Updates}{
	The \pkg{metafor} package is a work in progress and is updated on a regular basis with new functions and options. With \code{\link{metafor.news}}, you can read the \file{NEWS} file of the package after installation. Comments, feedback, and suggestions for improvements are very welcome.
	
	And since this is a frequently-asked-question: Functions for conducting multivariate meta-analyses and for handling correlated outcomes are currently under development and will be included in the package at a later point.
}
\author{
	Wolfgang Viechtbauer \cr
	e-mail: \email{wvb@www.wvbauer.com} \cr
	web: \url{http://www.wvbauer.com/} \cr

	Please post any questions about the package to the R-help mailing list (\url{https://stat.ethz.ch/mailman/listinfo/r-help}). Make sure you first read and follow the posting guide (\url{http://www.r-project.org/posting-guide.html}). To make sure that my mail filter catches your post, include the package name (i.e., \sQuote{metafor}) in your post.
}
\references{
	Cooper, H. C. & Hedges, L. V. (1994) \emph{The handbook of research synthesis}. New York: Russell Sage Foundation.

	Hedges, L. V. & Olkin, I. (1985) \emph{Statistical methods for meta-analysis}. San Diego, CA: Academic Press.

	Hedges, L. V. & Vevea, J. L. (1998) Fixed- and random-effects models in meta-analysis. \emph{Psychological Methods}, \bold{3}, 486--504.

	Laird, N. M. & Mosteller, F (1990). Some statistical methods for combining experimental results. \emph{International Journal of Technology Assessment in Health Care}, \bold{6}, 5--30.

	Mantel, N. & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. \emph{Journal of the National Cancer Institute}, \bold{22}, 719--748.

	Yusuf, S., Peto, R., Lewis, J., Collins, R. & Sleight, P. (1985) Beta blockade during and after myocardial infarction: An overview of the randomized trials. \emph{Progress in Cardiovascular Disease}, \bold{27}, 335--371.
}
\keyword{package}
