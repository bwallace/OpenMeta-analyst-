\name{HSROC}
\alias{HSROC}

\title{A function for joint meta-analysis of sensitivity and specificity of a diagnostic test.}

\description{ This function is used to estimate the parameters of a hierarchical summary receiver operating characteristic (HSROC) model allowing for the reference standard to be possibly imperfect, and assuming it is conditionally independent from the test under evaluation.  The estimation is carried out using a Gibbs sampler.  }

\usage{
HSROC(data, iter.num, init = NULL, sub_rs=NULL, 
  first.run = TRUE, path=getwd(), refresh = 100, 
  prior.SEref=NULL, prior.SPref=NULL, prior_PI=c(0,1), 
  prior_LAMBDA = c(-3,3), prior_THETA = c(-1.5,1.5), 
  prior_sd_alpha = list(0,2,"sd"), prior_sd_theta = list(0,2,"sd"), 
  prior_beta = c(-0.75,0.75)) 

}


\arguments{  
  \item{data}{a matrix with the number of rows equal to the number of studies and 4 columns.  Each row consists of the entries of the 2x2 table of the index test (i.e. test under evaluation) vs. the reference test reported in each study.  The ordering of the columns is ++, +-, -+, --, where the first entry refers to the result of the test under evaluation and the second entry refers to the result of the reference test. }
  \item{iter.num}{the number of iterations of the Gibbs sampler.}
  \item{init}{a list of initial values.  See details for further explanation.}	
  \item{sub_rs}{a list that describes the partition of the reference standards among the studies, if any.  The first element of the list is the number of different reference standards used.  Element 2 to \code{sub_rs[1]} are vectors of integers specifying the study numbers that used each reference test.  See details.}
  \item{prior.SEref}{a vector of values specifying the lower and upper limits of the sensitivity of the reference test(s) based on prior information.  If there are multiple reference standards, the lower and upper limits are each specified as a vector of length \code{sub_rs[1]}.  The default value is NULL, implying a single reference standard with perfect sensitivity. }
  \item{prior.SPref}{a vector of values specifying the lower and upper limits of the specificity of the reference test(s) based on prior information.  If there are multiple reference standards, the lower and upper limits are each specified as a vector of length \code{sub_rs[1]}.  The default value is NULL, implying a single reference standard with perfect specificity. }
  \item{first.run}{logical. If TRUE (default), the \code{gibbs.sampler} function is run for the first time.  If FALSE, the function assumes k iterations where already run and it continues from where it left off, that is from iteration k+1.}
  \item{path}{a character string pointing to the directory where the sample drawn at each iteration of the Gibbs sampler are to be stored.  }
  \item{refresh}{A positive integer defining the frequency at which the Gibbs sampler's progress will be displayed on the R GUI.  The default is 100.  }

  \item{prior_PI}{a vector with 2 components specifying the minimum and maximum values for the prevalence in each study  based on prior information.  If unspecified, \eqn{Beta(1,1)}{Beta(1,1)} priors are used.  }
  \item{prior_LAMBDA}{a vector with 2 components specifying the minimum and maximum values for the difference in mean values (diagnostic accuracy) among disease positive and negative groups based on prior information.  The default value is \code{c(-3,3)} implying a \eqn{U(-3,3)}{U(-3,3)}.}
  \item{prior_THETA}{a vector with 2 components specifying the minimum and maximum values for the overall mean cut-off value for defining a positive test based on prior information.  The default value is \code{c(-1.5,1.5)} implying a \eqn{U(-1.5,1.5)}{U(-1.5,1.5)}.}
  \item{prior_sd_alpha}{a list with 3 components.  The first 2 components are specifying the minimum and maximum values for the between study standard deviation in the difference in  mean values of the disease positive and negative groups in the \eqn{i^{th}}{ith} study, \eqn{\alpha_i}{alpha_i}, based on prior information.  The third component determine whether we want the prior to be on the standard deviation (sd) or the variance (v).  The default value is \code{list(0,2,"sd")} implying a \eqn{U(0,2)}{U(0,2)} prior. }
  \item{prior_sd_theta}{a list with 3 components. The first 2 components are specifying the minimum and maximum values for the between study standard deviation in the cut-off, \eqn{\theta_i}{theta_i}, in the \eqn{i^{th}}{ith} study based on prior information.  The third component determine whether we want the prior to be on the standard deviation (s) or the variance (v). The default value is \code{list(0,2,"sd")} implying a \eqn{U(0,2)}{U(0,2)} prior.  }
  \item{prior_beta}{a vector with 2 components specifying the minimum and maximum values for the logarithm of the ratio of the standard deviation of test results among patients with and without the disease, based on prior belief.  This parameter is assumed to be constant across studies.  The default value is \code{c(-0.75,0.75)} implying a \eqn{U(-0..75,0.75)}{U(-0.75,0.75)}.  If the argument is (\code{NULL}) the function assumes a range of (-log( (LAMBDA.up/3) + 1 ) , log( (LAMBDA.up/3) + 1 ) ), where LAMBDA.up is the upper limit of \code{prior.LAMBDA}. }


}

\details{

Our HSROC model uses a probit link function and not the logit link function used in the HSROC model developped by Rutter and Gatsonis (2001).  

The probability of a positive result on the index test for the \eqn{j^{th}}{jth} individual in the \eqn{i^{th}}{ith} study is given by :

\deqn{1 - \Phi( ( \theta_i - \alpha_i D_{ij} )exp(-\beta D_{ij}) ),}{1 - PHI( (theta_i - alpha_i D_{ij})*exp(-beta D_{ij})),}

while the probability of a negative result on the index test for the \eqn{j^{th}}{jth} individual in the \eqn{i^{th}}{ith} study is given by :

\deqn{\Phi( ( \theta_i - \alpha_i D_{ij} )exp(-\beta D_{ij}) ),}{PHI( (theta_i - alpha_i D_{ij})*exp(-beta D_{ij})),}

where \eqn{D_{ij}}{D_ij} = 0.5 if the \eqn{j^{th}}{jth} individual in the \eqn{i^{th}}{ith} study is disease positive and -0.5 if the \eqn{j^{th}}{jth} individual in the \eqn{i^{th}}{ith} study is not disease negative.


When ranges are provided for \code{prior.SEref}, \code{prior.SPref} and \code{prior_PI} they are transformed into Beta prior distributions using the method described in \code{beta.parameter}.

If the argument \code{init} = \code{NULL}, the function will randomly generate initial values based on the prior information.  Otherwise, the user can provide his own initial values for the within-study and between-study parameters and for the reference standards through the \code{init} argument, with the within-study, between study and reference standard initial values as the first, second and third element of \code{init}, respectively.  Furthermore, 
the within-study parameters must be a matrix-like object with each column being initial values for \eqn{\alpha_i}{alpha_i}, \eqn{\theta_i}{theta_i}, sensitivity of the test under evaluation \eqn{S_{1i}}{S1_i}, specificity of the test under evaluation \eqn{C_{1i}}{C1_i} and prevalence \eqn{\pi_i}{pi_i}.  
The between-study parameters must be a vector of the following initial values : \eqn{\Theta}{THETA}, \eqn{\sigma_{\theta}}{sigma_theta}, \eqn{\Lambda}{LAMBDA}, \eqn{\sigma_{\alpha}}{sigma_alpha} and \eqn{\beta}{beta}.  The initial values for the reference standard must be a 2 X \code{sub_rs[[1]]} matrix-like object.  
The first row must be the initial values of the sensitivity of the reference standard, while the second row must correspond to initial values of the specificity of the reference standard.  The ordering described above in the within-study, between-study and reference standard parameters must be preserved. 


The first element of the list-object \code{sub_rs} corresponds to the number of different reference standards. The default value is 1.  The number of additional elements will depend on the value of the first element.   There must be as many additional elements in \code{sub_rs} as there are different reference standards. Assuming the studies are labelled 1, ..., N, 
each of these additional elements must be a vector (possibly of length one) taking as their values the labelling of the corresponding studies sharing the same reference standard.  For example, if we have 2 reference tests, the first one aplied over study 1-10 and the second one applied over study 11-15 then the \code{sub_rs} list-argument should be of length 3 with the following elements : 3, 1:10, 11:15

The \code{path} argument points to the working directory where files written at each iteration of the Gibbs sampler will be saved.  If no path is provided, the current working directory will be used
}

\value{

Text files with samples from the joint posterior distribution of the between-study parameters, within-study parameters and performance parameters of the reference standard(s) are created in the \code{path} directory.  These results can be summarized using the \code{HSROCSummary} function.

The following files are also created and saved in the \code{path} directory :

\dQuote{Prior.information.txt}, lists the prior distributions.

\dQuote{Initial values.txt}, lists the initial values used.  If the argument \code{init} = \code{NULL}, the initial value file is called \dQuote{Random Initial values.txt}.

A series of files listing the values of various parameters sampled in the last iteration of the Gibbs sampler as follows :

\dQuote{REstarting values.txt}, contains values of the within-study parameters (\eqn{\alpha_i}{alpha_i}, \eqn{\theta_i}{theta_i}, sensitivity of test under evaluation ( \eqn{S_{1i}}{S1_i} ), specificity of test under evaluation ( \eqn{C_{1i}}{C1_i} ) and prevalence ( \eqn{\pi_i}{pi_i} ) ).

\dQuote{REstarting values 2.txt}, contains values of the between-study parameters (\eqn{\Lambda}{LAMBDA}, standard deviation of \eqn{\alpha_i}{alpha_i}, ( \eqn{\sigma_{\alpha}}{sigma_alpha} ), \eqn{\Theta}{THETA}, standard deviation of \eqn{\theta_i}{theta_i} ( \eqn{\sigma_{\theta}}{sigma_theta }) and \eqn{\beta}{beta} ). 

\dQuote{REstarting REFSTD.txt}, contains values of the sensitivity and specificity of the reference test.

\dQuote{REstart values index.txt}, lists the variable names in the 3 files described above.


}

\references{
N. Dendukuri, I. Schiller, L. Joseph and M. Pai \emph{Bayesian meta-analysis of the accuracy of a test for tuberculosis pleuritis in the absence of a gold-standard reference}. (Under review).

C. M. Rutter and C. A. Gatsonis. \emph{A hierarchical regression approach to meta-analysis of diagnostic accuracy evaluations}. Statistics in Medicine, 20(19):2865-2884, 2001.
}
  
\examples{

#===============================================================
#TO SET UP THE REFERENCE STANDARD
#===============================================================


#There were three different reference standards for the In.house dataset.  
#The first reference standard was used in study 1 and 2, 
#the second was used in studies 3 and 4 and the third in study 5 to 12. 
REFSTD = list(3, 1:2, 3:4, 5:11) 

#===============================================================
#TO SET UP DATA AND INITIAL VALUES
#===============================================================

data(In.house)
M = length(In.house[,1])


#Initial values for the within-study parameters
init.alpha = rep(2.5, M) ;	init.theta = rep(1, M) ;
init.s1 =  rep(0.5, M) ;	init.c1 = rep(0.5, M) ;
init.pi = rep(0.5, M)

#Initial values for the between-study parameters
init.THETA = 1 ;	init.sd.theta = 0.5 ;
init.LAMBDA = 2.5 ;	init.sd.alpha = 0.5 ;
init.beta = 0 ;

#Initial values for the reference standard sensitivities and specificities
init.s2 = rep(0.5, REFSTD[[1]]) ;	init.c2 = rep(0.5, REFSTD[[1]])

#The ordering of the initial values is important!
init1 = cbind(init.alpha, init.theta, init.s1, init.c1, init.pi)
init2 = c(init.THETA, init.sd.theta, init.LAMBDA, init.sd.alpha, init.beta)
init3 = rbind(init.s2, init.c2)

init = list(init1, init2, init3)
#===============================================================
#TO PROVIDE PRIOR INFORMATION
#===============================================================

S2.a = c(0.2, 0.2, 0.7) ; 	S2.b = c(0.6, 0.7, 0.9)
C2.a = rep(0.9, 3) ;	C2.b = rep(1, 3)

#===============================================================
#TO RUN GIBBS SAMPLER
#===============================================================



\dontrun{

set.seed(10)
estimates = HSROC(data=In.house, init=init, iter.num=5000,  
   prior.SEref=c(S2.a,S2.b), prior.SPref=c (C2.a,C2.b), sub_rs=REFSTD) 

#Putting prior information on sigma_alpha^2 (sigma_alphs squared) 
#instead of sigma_alpha
set.seed(10)
estimates = HSROC(data=In.house, init=init, iter.num=5000,  
   prior.SEref=c(S2.a,S2.b), prior.SPref=c (C2.a,C2.b), 
		sub_rs=REFSTD, prior_sd_alpha = list(0,2,"v"))


#Letting the function select randomly its own initial values
set.seed(10)
estimates = HSROC(data=In.house, iter.num=5000,  
   prior.SEref=c(S2.a,S2.b), prior.SPref=c (C2.a,C2.b), 
		sub_rs=REFSTD)

}
}

\keyword{models}

