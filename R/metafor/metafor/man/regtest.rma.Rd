\name{regtest.rma}
\alias{regtest.rma}
\title{Regression Tests for Funnel Plot Asymmetry for rma Objects}
\description{The function can be used to carry out various tests for funnel plot asymmetry, including Egger's regression test and variations thereof, for objects of class \code{"rma"}.}
\usage{
\method{regtest}{rma}(x, model="rma", predictor="sei", ni=NULL, \dots)
}
\arguments{
	\item{x}{an object of class \code{"rma"}.}
	\item{model}{either \code{"rma"} or \code{"lm"} to indicate the type of model to use for the regression test. See \sQuote{Details}.}
	\item{predictor}{either \code{"sei"} \code{"vi"}, \code{"ni"}, or \code{"ninv"} to indicate the type of independent variable for the regression test. See \sQuote{Details}.}
	\item{ni}{the total sample size. Only needed when carrying out a regression test with either \code{predictor="ni"} or \code{predictor="ninv"} and the object \code{x} does not already contain this information.}
	\item{\dots}{other arguments.}
}
\details{
	Various tests for funnel plot asymmetry have been suggested in the literature, including the rank correlation test by Begg and Mazumdar (1994) and the regression test by Egger et al. (1997). Extensions, modifications, and further developments of the regression test are described (among others) by Macaskill, Walter, and Irwig (2001), Sterne and Egger (2005), Harbord, Egger, and Sterne (2006), Peters et al. (2006), Ruecker, Schwarzer, and Carpenter (2008), and Moreno et al. (2009). The various versions of the regression test differ in terms of the model (either a regular weighted regression with a multiplicative dispersion term or one of the meta-analytic models is used), in terms of the independent variable that the observed outcomes are hypothesized to be related to when publication bias is present (suggested predictors include the standard error, the sampling variance, the total sample size, and the inverse of the total sample size), and in terms of the outcome measure used (e.g., for 2x2 table data, one has the choice between various outcome measures). The idea behind the various tests is the same though: If there is a relationship between the observed outcomes and the chosen predictor, then this usually implies asymmetry in the funnel plot and may be an indication of publication bias.

	The \code{regtest.rma} function can be used to carry out various versions of the regression test. The model is chosen via the \code{model} argument, with \code{model="lm"} for weighted regression with a multiplicative dispersion term or \code{model="rma"} for the meta-analytic models. In the latter case, arguments such as \code{method}, \code{weighted}, and \code{knha} used during the initial model fitting are also used for the regression test. Therefore, if one wants to conduct the regression test with a random-effects model, one should first fit a model with, for example, \code{method="REML"} and then use the \code{regtest.rma} function.

	The predictor is chosen via the \code{predictor} argument, with \code{predictor="sei"} for the standard error, \code{predictor="vi"} for the sampling variance, \code{predictor="ni"} for the total sample size, and \code{predictor="ninv"} for the inverse of the total sample size. The object \code{x} will contain information about the total sample size when \code{measure} was \emph{not} equal to \code{"GEN"} during the initial model fitting.

	Finally, depending on what outcome measure was used for the model fitting will determine which outcome measure is used for the regression test.
}
\value{
	An object of class \code{"regtest.rma"}. The object is a list containing the following components:	
	\item{type}{the type of test.}
	\item{model}{the model used for the regression test.}
	\item{predictor}{the predictor used for the regression test.}
	\item{zval}{the value of the test statistic.}
	\item{pval}{the corresponding p-value}
	\item{dfs}{the degrees of freedom of the test stastistic (if the test is based on a t-distribution).}

	The results are formated and printed with the \code{\link{print.regtest.rma}} function.
}
\note{
	All of the tests do not directly test for publication bias, but for a relationship between the observed outcomes and the chosen predictor. If such a relationship is present, then this usually implies asymmetry in the funnel plot and may be an indication of publication bias. However, it is important to keep in mind that there can be other reasons besides publication bias that could lead to asymmetry in the funnel plot.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Begg, C. B. & Mazumdar, M. (1994) Operating characteristics of a rank correlation test for publication bias. \emph{Biometrics}, \bold{50}, 1088--1101.

	Egger, M., Davey Smith, G., Schneider, M. & Minder, C. (1997) Bias in meta-analysis detected by a simple, graphical test. \emph{British Medical Journal}, \bold{315}, 629--634.

	Harbord, R. M., Egger, M. & Sterne, J. A. C. (2006). A modified test for small-study effects in meta-analyses of controlled trials with binary endpoints. \emph{Statistics in Medicine}, \bold{25}, 3443--3457.

	Macaskill, P., Walter, S. D. & Irwig, L. (2001) A comparison of methods to detect publication bias in meta-analysis. \emph{Statistics in Medicine}, \bold{20}, 641--654.

	Moreno, S. G., Sutton, A. J., Ades, A. E., Stanley, T. D., Abrams, K. R., Peters, J. L. & Cooper, N. J. (2009) Assessment of regression-based methods to adjust for publication bias through a comprehensive simulation study. \emph{BMC Medical Research Methodology}, \bold{9}:2.

	Peters, J. L., Sutton, A. J., Jones, D. R., Abrams, K. R. & Rushton, L. (2006) Comparison of two methods to detect publication bias in meta-analysis. \emph{Journal of the American Medical Association}, \bold{295}, 676--680.

	Ruecker, G., Schwarzer, G. & Carpenter, J. (2008) Arcsine test for publication bias in meta-analyses with binary outcomes. \emph{Statistics in Medicine}, \bold{27}, 746--763.

	Sterne, J. A. C. & Egger, M. (2005). Regression methods to detect publication and other bias in meta-analysis. In H. R. Rothstein, A. J. Sutton & M. Borenstein (Eds.) (2005) \emph{Publication bias in meta-analysis: Prevention, assessment, and adjustments} (pp. 99--110). Chichester, England: Wiley.
}
\seealso{
	\code{\link{regtest}}, \code{\link{ranktest}}
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
dat <- cbind(dat.bcg, dat)

### random-effects model
res <- rma(yi, vi, data=dat, method="FE")
regtest(res)

res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat, method="REML")
regtest(res, model="lm", predictor="ni")

res <- rma(measure="PETO", ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat, method="FE")
regtest(res, predictor="ninv")

res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           data=dat, mods=cbind(ablat), method="REML")
regtest(res, predictor="ninv")
}
\keyword{htest}
