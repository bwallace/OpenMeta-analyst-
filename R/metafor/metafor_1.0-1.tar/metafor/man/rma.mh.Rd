\name{rma.mh}
\alias{rma.mh}
\title{Meta-Analysis via the Mantel-Haenszel Method}
\description{Function to fit a fixed-effects model for data from 2x2 tables via the Mantel-Haenszel method.}
\usage{
rma.mh(ai, bi, ci, di, n1i, n2i, data=NULL, 
       slab=NULL, subset=NULL, measure="OR", 
       add=c(1/2,0), to=c("only0","none"), level=95, digits=4)
}
\arguments{
	\item{ai}{vector to specify the 2x2 table frequencies (upper left cell). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{bi}{vector to specify the 2x2 table frequencies (upper right cell). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{ci}{vector to specify the 2x2 table frequencies (lower left cell). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{di}{vector to specify the 2x2 table frequencies (lower right cell). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{n1i}{vector to specify the group sizes or row totals (first group). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{n2i}{vector to specify the group sizes or row totals (second group). See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{data}{an optional data frame containing the variables given to the arguments above.}
	\item{slab}{an optional vector with unique labels for the \eqn{k} studies.}
	\item{subset}{an optional vector indicating the subset of tables that should be used for the analysis. This can be a logical vector of length \eqn{k} or a numeric vector indicating the indices of the tables to include.}
	\item{measure}{a character string indicating the outcome measure to use for the meta-analysis: the odds ratio (\code{"OR"}), relative risk (\code{"RR"}), or risk difference (\code{"RD"}).}
	\item{add}{vector with two numbers indicating the amount to add to the cell frequencies of the 2x2 table. The first number is used in the calculation of the individual outcomes, the second number is used when applying the Mantel-Haenszel method. See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{to}{a character vector with two strings indicating when the values under \code{add} should be added. When set to \code{"all"}, the value of \code{add} is added to each cell of the 2x2 tables in all \eqn{k} tables. When set to \code{"only0"}, the value of \code{add} is added to each cell of the 2x2 tables with at least one cell equal to 0. When set to \code{"if0all"}, the value of \code{add} is added to each cell of the 2x2 tables in all \eqn{k} studies, but only when there is at least one 2x2 table with a zero entry. Setting \code{to="none"} or \code{add=0} has the same effect: No adjustment to the observed table frequencies is made. The first string again applies when calculating the individual outcomes, the second string when applying the Mantel-Haenszel method. See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{level}{a numerical value between 0 and 100 specifying the confidence interval level (default is 95).}
	\item{digits}{an integer specifying the number of decimal places to which the printed results should be rounded (default is 4).}
}
\details{
	The studies providing data in terms of 2x2 tables are assumed to be of the form:
	\tabular{lccc}{
	        \tab outcome 1 \tab outcome 2 \tab total \cr
	group 1 \tab \code{ai} \tab \code{bi} \tab \code{n1i} \cr
	group 2 \tab \code{ci} \tab \code{di} \tab \code{n2i} \cr
	}
	where \code{ai}, \code{bi}, \code{ci}, and \code{di} denote the cell frequencies and \code{n1i} and \code{n2i} the row totals. For example, in a set of RCTs, group 1 and group 2 may refer to the treatment and placebo group, with outcome 1 denoting some event of interest and outcome 2 its complement. In a set of case-control studies, group 1 and group 2 may refer to the group of cases and the group of controls, with outcome 1 denoting, for example, exposure to some risk factor and outcome 2 non-exposure.

	An approach for aggregating 2x2 table data of this type was suggested by Mantel and Haenszel (1959), which can be used in combination with the odds ratio, relative risk, or risk difference (\code{measure="OR"}, \code{"RR"}, or \code{"RD"}). The Mantel-Haenszel method provides a weighted estimate under a fixed-effects model. The method is particularly advantageous when aggregating a large number of tables with small sample sizes (the so-called sparse data or increasing strata case). When analyzing odds ratios, the Cochran-Mantel-Haenszel test and Tarone's test for heterogeneity are also provided. When analyzing odds ratios or relative risks, the printed results are given both in terms of the log and the raw units (for easier interpretation).

	The Mantel-Haenszel method itself does not require the calculation of the individual outcome values and directly makes use of the 2x2 table counts. Zero cells are not a problem (except in extreme cases, such as when one of the two outcomes never occurs in any of the tables). Therefore, it is also unnecessary to add some constant to the cell counts when there are zero cells. However, for plotting and various other functions, it is necessary to calculate the individual outcome values for the \eqn{k} tables. Here, zero cells can be problematic, so adding a constant value to the cell counts ensures that all \eqn{k} values can be calculated. The \code{add} and \code{to} arguments are used to specify what value should be added to the 2x2 cell frequencies and under what circumstances when calculating the individual outcome values and when applying the Mantel-Haenszel method.
}
\value{
	An object of class \code{c("rma.mh","rma")}. The object is a list containing the following components:
	\item{b}{aggregated log odds ratio, log relative risk, or risk difference.}
	\item{se}{standard error of the aggregated value.}
	\item{zval}{test statistics of the aggregated value.}
	\item{pval}{p-value for the test statistic.}
	\item{ci.lb}{lower bound of the confidence interval.}
	\item{ci.ub}{upper bound of the confidence interval.}
	\item{QE}{test statistic for the test of heterogeneity.}
	\item{QEp}{p-value for the test of heterogeneity.}
	\item{CMH}{Cochran-Mantel-Haenszel test statistic (only when \code{measure="OR"}).}
	\item{CMHp}{corresponding p-value (only when \code{measure="OR"}).}
	\item{TA}{Tarone's heterogeneity test statistic (only when \code{measure="OR"}).}
	\item{TAp}{corresponding p-value (only when \code{measure="OR"}).}
	\item{k}{number of tables included in the analysis.}
	\item{yi, vi}{the vector of individual outcomes and corresponding sampling variances.}
	\item{fit.stats}{a list with the log likelihood, deviance, AIC, and BIC values under the unrestricted and restricted likelihood.}
	\item{\dots}{some additional elements/values.}

	The results of the fitted model are neatly formated and printed with the \code{\link{print.rma.mh}} function. If you also want the fit statistics, use \code{\link{summary.rma}} (or use the \code{\link{fitstats.rma}} function to extract them).

	The \code{\link{residuals.rma}}, \code{\link{rstandard.rma.mh}}, and \code{\link{rstudent.rma.mh}} functions extract raw and standardized residuals. Leave-one-out diagnostics can be obtained with \code{\link{leave1out.rma.mh}}.

	Forest, funnel, and radial plots of the individual outcomes can be obtained with \code{\link{forest.rma}}, \code{\link{funnel.rma}}, and \code{\link{radial.rma}}. The \code{\link{qqnorm.rma.mh}} function provides a normal QQ plot of the standardized residuals. One can also just call \code{\link{plot.rma.mh}} on the fitted model object to obtain various plots at once.

	A cumulative meta-analysis (i.e., adding one obervation at a time) can be obtained with \code{\link{cumul.rma.mh}}.

	Other assessor functions include \code{\link{coef.rma}}, \code{\link{vcov.rma}}, and \code{\link{logLik.rma}}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Mantel, N. & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. \emph{Journal of the National Cancer Institute}, \bold{22}, 719--748.
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the (log) odds ratios using the Mantel-Haenszel method
rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="OR")

### meta-analysis of the (log) relative risks using the Mantel-Haenszel method
rma.mh(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR")
}
\keyword{models}
