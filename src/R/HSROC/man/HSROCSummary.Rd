\name{HSROCSummary}
\alias{HSROCSummary}

\title{Summary statistics for HSROC models.}
\description{
  This function produces summaries for the HSROC model parameters
}

\usage{

HSROCSummary(data, burn_in = 0, iter.keep = NULL, Thin = 1, sub_rs=NULL, 
  point_estimate = c("median", "mean"), path = getwd(), chain = NULL,
  tv = NULL, digit = 6, print_plot = FALSE ) 

}


\arguments{  
  \item{data}{a matrix with the number of rows equal to the number of studies and 4 columns.  Each row consists of the entries of the 2x2 table of the index test (i.e. test under evaluation) vs. the reference test reported in each study.  The ordering of the columns is ++, +-, -+, --, where the first entry refers to the result of the test under evaluation and the second entry refers to the result of the reference test. }
  \item{burn_in}{The number of early iterations that are to be dropped.  The default value is 0.   }
  \item{iter.keep}{ Maximum number of iteration we want to keep. }
  \item{Thin}{a single numeric value.  It sets the numerical field used to select every thin-th iteration to contribute to the estimates being calculated.  The default value is 1. }
  \item{sub_rs}{a list that describes the partition of the reference standard among the studies, if any.  See details for further explanations.}
  \item{point_estimate}{a character string indicating which method is to be used to calculate the estimates.  One of "median" (default) or "mean", can be used }
  \item{path}{a character string pointing to the directory where the SUMMARY files are to be stored.  }
  \item{chain}{ A list of character strings pointing to each directory, one for each chain, where the files created during the Gibbs sampler process are stored.}
  \item{tv}{a list of true parameter values.  See details for further explanations }
  \item{digit}{integer indicating the number of decimal places to be used.  The default value is 6.  }
  \item{print_plot}{logical.  If TRUE, pdf files of trace, density and summary receiver operating characteristic (SROC) curve plots are saved in the \code{path} working directory to help assess convergence of the Gibbs sampler.  }
}

\details{

The first element of the list-object \code{sub_rs} corresponds to the number of different reference standards. The default value is 1.  The number of additional elements will depend on the value of the first element.   There must be as many additional element in \code{sub_rs} as there are different reference standards. Assuming the studies are labelled 1, ..., N, 
each of these additional elements must be a vector (possibly of length one) taking as their values the labelling of the corresponding studies sharing the same reference standard.  For example, if we have 2 reference tests, the first one aplied over study 1-10 and the second one applied over study 11-15 then the \code{sub_rs} list-argument should be of length 3 with the following elements : 3, 1:10, 11:15


If the argument \code{tv} is equal to \code{NULL}, the function assumes the data are coming from a real life example.  Otherwise, it assumes the data are coming from a simulated dataset in which case the user must provide the \dQuote{true parameters} that were used to simulate the data for the within-study and between-study parameters and for the reference standards through the \code{tv} argument.  
The within-study parameters must be a matrix-like object with each column being true values for \eqn{\alpha_i}{alpha_i}, \eqn{\theta_i}{theta_i}, sensitivity of test under evaluation \eqn{S_{1i}}{S1_i}, specificity of test under evaluation \eqn{C_{1i}}{C1_i} and prevalence \eqn{\pi_i}{pi_i}.  
The between-study parameters must be a vector of the following true values : \eqn{\Theta}{THETA}, \eqn{\sigma_{\theta}}{sigma_theta}, \eqn{\Lambda}{LAMBDA}, \eqn{\sigma_{\alpha}}{sigma_alpha} and \eqn{\beta}{beta}.  The reference stadard initial values must be a 2 X \code{sub_rs[[1]]} matrix-like object.  
The first row must be the true values of the sensitivity of the reference standard, while the second row must correspond to true values of the specificity of the reference standard.   The ordering described above in the within-study, between-study and reference standard true parameters must be preserved otherwise an error message will be displayed. 

If the argument \code{print_plot} is equal to \code{TRUE}, the function will create and save in the \code{path} working directory 3 type of plots to help the user judge if the descriptive statistics are reliable. First, a trace plot for each parameter will be created to help evaluate whether the Gibbs sampler has converged. Each trace plot is a scatter plot of the posterior sample of a single parameter vs the iteration number of the Gibbs sampler. Second, a density plot for each parameter will also be created.  It plots a smoothed posterior kernel density estimate for each parameter.  Finally, a SROC curve plot will also be created by the function. It summarizes the performance of diagnostic tests by plotting the relationship between the true positive rate and the false positive rate of the tests, as the threshold used to distinguish disease cases from noncases varies.   

}

\value{

A list of : Point estimates and \eqn{95\%} highest posterior density (HPD) intervals of the between-study parameters, within-study parameters and reference standard parameters.  All estimates are obtained via a Gibbs sampler process.

It also prints in the \code{path} working directory a text file with a more complete summary of the results listed above (i.e. the results also include stadard errors and MC errors plus the posterior predictive value for the sensitivity and specificity of a new study that has not yet taken place).  

}


\examples{

#REAL-LIFE EXAMPLES
#
#PLEASE NOTE THAT BOTH EXAMPLES BELOW ASSUME THE EXISTANCE OF POSTERIOR SAMPLES OBTAINED FROM THE 'HSROC' FUNCTION.  
#IN OTHER WORDS' ONE NEEDS TO RUN THE 'HSROC' FUNCTION BEFORE USING THE 'HSROCSUmmary' FUNCTION.
#
#Example 1
#To get descriptive statistics and graphical summaries for the MRI data 
#(Scheidler et al. 1997) after dropping the first 5,000 iterations.

data(MRI)	#load the data
\dontrun{
HSROCSummary(data = MRI, burn_in=5000, print_plot=TRUE )
}


#Example 2
#To get descriptive statistics and graphical summaries for the In.house 
#data (Pai et al. 2004) coming from 2 different chains.  
#We provide the path to each chain's directory, i.e. the directory where 
#all files created during the Gibbs sampler process are stored for 
#each chain.  Let's assume there are two fictional directoies 
#chain_path = list("C:/path_to_chain_1", "C:/path_to_chain_2").
#Let's assume we drop the first 5,000 iterations and we use a thinning 
#interval of 10. 


data(In.house)	#load the data
\dontrun{
HSROCSummary(data = In.house, burn_in=5000, Thin=10, 
		chain=chain_path, print_plot=TRUE )
}

\dontshow{

x <- rnorm(1000)
y <- as.mcmc(x)	
z <- HPDinterval(y)

}

}


\references{ 

Scheidler J, Hricak H, Yu KK, Subak L, Segal MR. \emph{Radiological evaluation of lymph node metastases in patients with cervical cancer : a meta-analysis}.  Journal of the American Medical Association 1997 ; 278(13):1096-1101. 

Pai, M. et al. (2004) \emph{Nucleic acid amplification in the diagnosis of tuberculous pleuritis: a systematic review and meta-analysis}.  BMC Infect Dis 2004, 4:6.

}


\keyword{models}

