\name{dat.bcg}
\docType{data}
\alias{dat.bcg}
\title{Data for BCG Vaccine Studies}
\description{Results from 13 clinical trials examining the effectiveness of the bacillus Calmette-Guerin (BCG) vaccine for preventing tuberculosis.}
\usage{dat.bcg}
\format{The data frame contains the following columns:
\tabular{lll}{
\bold{trial}  \tab \code{numeric}   \tab trial number \cr
\bold{author} \tab \code{character} \tab author(s) \cr
\bold{year}   \tab \code{numeric}   \tab publication year \cr
\bold{tpos}   \tab \code{numeric}   \tab number of TB positive cases in the treated (vaccinated) group \cr
\bold{tneg}   \tab \code{numeric}   \tab number of TB negative cases in the treated (vaccinated) group \cr
\bold{cpos}   \tab \code{numeric}   \tab number of TB positive cases in the control group \cr
\bold{cneg}   \tab \code{numeric}   \tab number of TB negative cases in the control group \cr
\bold{ablat}  \tab \code{numeric}   \tab absolute latitude where the study was conducted \cr
\bold{alloc}  \tab \code{character} \tab method of treatment allocation (random, alternate, or systematic)
}
}
\details{
	The 13 studies provide data in terms of 2x2 tables in the form:
	\tabular{lcc}{
	                 \tab TB positive \tab TB negative \cr
	vaccinated group \tab \code{tpos} \tab \code{tneg} \cr
	control group    \tab \code{cpos} \tab \code{cneg} \cr
	}
	The goal of the meta-analysis was to examine the overall effectiveness of the BCG vaccine for preventing tuberculosis and to examine moderators that may potentially influence the size of the effect.

	The data set has been used in several publications to illustrate meta-analytic methods (see \sQuote{References}).
}
\source{
	Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E., Burdick, E., Fineberg, H. V. & Mosteller, F. (1994) Efficacy of BCG vaccine in the prevention of tuberculosis: Meta-analysis of the published literature. \emph{Journal of the American Medical Association}, \bold{271}, 698--702.
}
\references{
	Berkey, C. S., Hoaglin, D. C., Mosteller, F. & Colditz, G. A. (1995). A random-effects regression model for meta-analysis. \emph{Statistics in Medicine}, \bold{14}, 395--411.

	van Houwelingen, H. C., Arends, L. R. & Stijnen, T. (2002). Advanced methods in meta-analysis: Multivariate approach and meta-regression. \emph{Statistics in Medicine}, \bold{21}, 589--624.
}
\examples{
### load BCG vaccine data
data(dat.bcg)

### meta-analysis of the log risk ratios using a random-effects model
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           measure="RR", data=dat.bcg, method="DL")
res

### average risk ratio with 95\% CI
predict(res, transf=exp)

### mixed-effects model with absolute latitude as a moderator
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, 
           mods=ablat, measure="RR", data=dat.bcg, method="DL")
res

### predicted average risk ratios for 10-60 degrees absolute latitude
predict(res, newmods=c(10, 20, 30, 40, 50, 60), transf=exp)
}
\keyword{datasets}
