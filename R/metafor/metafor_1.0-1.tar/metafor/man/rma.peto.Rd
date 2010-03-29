\name{rma.peto}
\alias{rma.peto}
\title{Meta-Analysis via Peto's Method}
\description{Function to fit a fixed-effects model for data from 2x2 tables via Peto's method.}
\usage{
rma.peto(ai, bi, ci, di, n1i, n2i, data=NULL, 
         slab=NULL, subset=NULL, 
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
	\item{add}{vector with two numbers indicating the amount to add to the cell frequencies of the 2x2 table. The first number is used in the calculation of the individual outcomes, the second number is used when applying Peto's method. See below and the documentation of the \code{\link{escalc}} function for more details.}
	\item{to}{a character vector with two strings indicating when the values under \code{add} should be added. When set to \code{"all"}, the value of \code{add} is added to each cell of the 2x2 tables in all \eqn{k} tables. When set to \code{"only0"}, the value of \code{add} is added to each cell of the 2x2 tables with at least one cell equal to 0. When set to \code{"if0all"}, the value of \code{add} is added to each cell of the 2x2 tables in all \eqn{k} studies, but only when there is at least one 2x2 table with a zero entry. Setting \code{to="none"} or \code{add=0} has the same effect: No adjustment to the observed table frequencies is made. The first string again applies when calculating the individual outcomes, the second string when applying Peto's method. See below and the documentation of the \code{\link{escalc}} function for more details.}
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

	An approach for aggregating 2x2 table data of this type was suggested by Peto (see Yusuf et al., 1985). The method provides a weighted estimate of the log odds ratio under a fixed-effects model. Note that the printed results are given both in terms of the log and the raw units (for easier interpretation).

	The method itself does not require the calculation of the individual log odds ratios and directly makes use of the 2x2 table counts. Zero cells are not a problem (except in extreme cases, such as when one of the two outcomes never occurs in any of the tables). Therefore, it is also unnecessary to add some constant to the cell counts when there are zero cells. However, for plotting and various other functions, it is necessary to calculate the individual log odds ratios for the \eqn{k} tables. Here, zero cells can be problematic, so adding a constant value to the cell counts ensures that all \eqn{k} values can be calculated. The \code{add} and \code{to} arguments are used to specify what value should be added to the 2x2 cell frequencies and under what circumstances when calculating the individual log odds ratios and when applying Peto's method.
}
\value{
	An object of class \code{c("rma.peto","rma")}. The object is a list containing the following components:
	\item{b}{aggregated log odds ratio.}
	\item{se}{standard error of the aggregated value.}
	\item{zval}{test statistics of the aggregated value.}
	\item{pval}{p-value for the test statistic.}
	\item{ci.lb}{lower bound of the confidence interval.}
	\item{ci.ub}{upper bound of the confidence interval.}
	\item{QE}{test statistic for the test of heterogeneity.}
	\item{QEp}{p-value for the test of heterogeneity.}
	\item{k}{number of tables included in the analysis.}
	\item{yi, vi}{the vector of individual log odds ratios and corresponding sampling variances.}
	\item{fit.stats}{a list with the log likelihood, deviance, AIC, and BIC values under the unrestricted and restricted likelihood.}
	\item{\dots}{some additional elements/values.}

	The results of the fitted model are neatly formated and printed with the \code{\link{print.rma.peto}} function. If you also want the fit statistics, use \code{\link{summary.rma}} (or use the \code{\link{fitstats.rma}} function to extract them).

	The \code{\link{residuals.rma}}, \code{\link{rstandard.rma.peto}}, and \code{\link{rstudent.rma.peto}} functions extract raw and standardized residuals. Leave-one-out diagnostics can be obtained with \code{\link{leave1out.rma.peto}}.

	Forest, funnel, and radial plots of the individual outcomes can be obtained with \code{\link{forest.rma}}, \code{\link{funnel.rma}}, and \code{\link{radial.rma}}. The \code{\link{qqnorm.rma.peto}} function provides a normal QQ plot of the standardized residuals. One can also just call \code{\link{plot.rma.peto}} on the fitted model object to obtain various plots at once.

	A cumulative meta-analysis (i.e., adding one obervation at a time) can be obtained with \code{\link{cumul.rma.peto}}.

	Other assessor functions include \code{\link{coef.rma}}, \code{\link{vcov.rma}}, and \code{\link{logLik.rma}}.
}
\author{Wolfgang Viechtbauer; \email{wvb@www.wvbauer.com}; \url{http://www.wvbauer.com/}}
\references{
	Yusuf, S., Peto, R., Lewis, J., Collins, R. & Sleight, P. (1985) Beta blockade during and after myocardial infarction: An overview of the randomized trials. \emph{Progress in Cardiovascular Disease}, \bold{27}, 335--371.
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the (log) odds ratios using Peto's method
rma.peto(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
}
\keyword{models}
