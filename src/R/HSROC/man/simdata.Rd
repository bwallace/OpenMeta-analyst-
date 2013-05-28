\name{simdata}
\alias{simdata}

\title{Simulate a dataset based on a HSROC model}
\description{
  This function simulates a dataset based on the HSROC diagnostic meta-analysis model.  It allows for the reference standard to be imperfect or perfect. 
}
\usage{
simdata(N, n, n.random = "FALSE", sub_rs = NULL, prev, se_ref = NULL, 
   sp_ref = NULL, T, range.T = c(-Inf, Inf), L, range.L = c(-Inf, Inf), 
   sd_t, sd_a, b, path = getwd()  )
}


\arguments{
 \item{N}{the number of studies to be included in the meta-analysis.}  
 \item{n}{numerical vector, possibly a single value, specifying the number of individuals within each study.  See details for further explanations.  }  
 \item{n.random}{if TRUE, the number of individuals within each study is drawn from n with replacement.}  
 \item{sub_rs}{a list that specifies the reference standard used by each study.  See details for further explanations.}
 \item{prev}{a vector of length \code{N} giving the prevalence in each study.}	
 \item{se_ref}{a vector of length equal to the number of reference standards giving the sensitivity for each reference test.}	
 \item{sp_ref}{a vector of length equal to the number of reference standards giving the specificity for each reference test.}	
 \item{T}{single numeric value, the overall mean cut-off value to define a positive test.}
 \item{range.T}{a vector of length 2 specifiying a range of values for the individual cut-off \eqn{\theta_i}{theta_i}.  See details for further explanations.  }	
 \item{L}{single numeric value, the overall difference in mean values (diagnostic accuracy) on the continuous index test result comparing the diseased group and the non-diseased group.}	
 \item{range.L}{a vector of length 2 specifiying a range of values for the individual difference in mean values (diagnostic accuracy) on the continuous index test result comparing the diseased group and the non-diseased group \eqn{\alpha_i}{alpha_i}.  See details for further explanations.  }
 \item{sd_t}{single numeric value, the between study standard deviation in the cut-off \eqn{\theta_i}{theta_i}.  }	
 \item{sd_a}{single numeric value, the between study standard deviation in the mean value of the index test disease group \eqn{\alpha_i}{alpha_i}  }	
 \item{b}{single numeric value, the ratio of the continuous standard deviation of the index test results on patients with the disease compared to patients without the disease. }	
 \item{path}{a character string pointing to the directory where the simulated data will be saved to.  }

}


\value{
A list of the 2x2 tables for each study, the between-study parameters, the within-study parameters and the reference standard.

Text files are created in the \code{path} directory.  These files are :

\dQuote{True_values.txt}, reports the within-study parameters \eqn{\alpha_i}{alpha_i}, \eqn{\theta_i}{theta_i}, sensitivity of test under evaluation ( \eqn{S_{1i}}{S1_i} ), specificity of test under evaluation ( \eqn{C_{1i}}{C1_i} ) and prevalence (\eqn{\pi_i}{pi_i}) used in the simulation.

\dQuote{True_values2.txt}, reports the values of the between-study parameters \eqn{\Lambda}{LAMBDA}, standard deviation of \eqn{\alpha_i}{alpha_i} ( \eqn{\sigma_{\alpha}}{sigma_alpha} ), \eqn{\Theta}{THETA}, standard deviation of \eqn{\theta_i}{theta_i} ( \eqn{\sigma_{\theta}}{sigma_theta} ) and \eqn{\beta}{beta} used to simulate the data. 

\dQuote{True_REFSTD.txt}, reports the values of the reference standard used to simulate the data.

\dQuote{True_values_index.txt}, reports the variable names of the 3 files described above.

		. 
}


\details{

The HSROC model uses the following parametrization : \eqn{S_i = \Phi(\frac{-\theta_i + \alpha_i/2}{exp(\beta/2)})}{S_i = Phi(-(theta_i - alpha_i/2)/exp(beta/2))} and \eqn{C_i = \Phi(\frac{\theta_i + \alpha_i/2}{exp(-\beta/2)})}{C_i = Phi((theta_i + alpha_i/2)/exp(-beta/2))}

If \code{n.random} is \code{FALSE}, the number of components in \code{n} must match the value of \code{N}, unless \code{n} is equal to a single value.  For the latter case, all studies would be assumed to have the same number of individuals, that is \code{n}.
If \code{n.random} is \code{TRUE},  the number of elements may not necessarly be equal to the value of \code{N}.

The first element of the list-object \code{sub_rs} corresponds to the number of different reference standards. The default value is 1.  The number of additional elements will depend on the value of the first element.   There must be as many additional elements in \code{sub_rs} as there are different reference standards. Assuming the studies are labelled 1, ..., N, 
each of these additional elements must be a vector (possibly of length one) taking as their values the labels of the studies sharing the same reference standard.  For example, if we have 2 reference tests, the first one applied over studies 1-10 and the second one applied over studies 11-15 then the \code{sub_rs} list-argument should be of length 3 with the following elements : 2, 1:10, 11:15
  
The \code{range.T} argument ensures the individual \eqn{\theta_i}{theta_i} will be generated within the range provided.  If no range is provided by the user (default) the function assumes no restrictions are made on the possible values of \eqn{\theta_i}{theta_i}.
The \code{range.L} argument ensures the individual \eqn{\alpha_i}{alpha_i} will be generated within the range provided.  If no range is provided by the user (default) the function assumes no restrictions are made on the possible values of \eqn{\theta_i}{theta_i}.

}


\examples{

#EXAMPLE 1
#We want to simulate data for 10 studies based on an HSROC model assuming 
#each study uses the same imperfect reference standard.
 
N = 10
LAMBDA = 2
sd_alpha = 0.75
THETA = 1.5
sd_theta = 0.5
beta = 0
pi = runif(10,0,1)

REFSTD = list(1, 1:10)  #Only 1 reference standard ...
s2 = c(0.5)	 #Sensitivity of the reference test
c2 = c(0.85) 	 #Specificity of the reference test	


sim.data = simdata(N=N, n = c(50,50,60,60,70,70,80,80,90,90), 
   sub_rs = REFSTD, prev=pi, se_ref=s2, sp_ref=c2, T=THETA, 
   L=LAMBDA, sd_t=sd_theta, sd_a=sd_alpha, b=beta)



#EXAMPLE 2
#We want to simulate data for 15 studies based on an HSROC model such that 
#the first 5 studies share a common reference standard and the remaining 
#10 studies also share a common reference standard.
 
N = 15
LAMBDA = 3.6
sd_alpha = 1.15
THETA = 2.3
sd_theta = 0.75
beta = 0.15
pi = runif(15,0.1,0.5)

REFSTD = list(2, 1:5, 6:15)  #Two different reference standards ...
s2 = c(0.40, 0.6)	 #Sensitivity of the reference tests
c2 = c(0.75,0.95) 	 #Specificity of the reference tests	

#Thus, for the first 5 studies, S2 = 0.40 and C2 = 0.75 while for the last 
#10 studies s2 = 0.6 and c2 = 0.95


sim.data = simdata(N=N, n=seq(30,120,1), n.random=TRUE, sub_rs = REFSTD, 
   prev=pi,  se_ref=s2, sp_ref=c2, T=THETA, L=LAMBDA, sd_t=sd_theta, 
   sd_a=sd_alpha, b=beta)


#EXAMPLE 3
#Assume the same context as the one in EXAMPLE 2 and let's suppose
#that each individual cut-off theta_i should lie between [-5,5] 
 
N = 15
LAMBDA = 3.6
sd_alpha = 1.15
THETA = 2.3
sd_theta = 0.75
beta = 0.15
pi = runif(15,0.1,0.5)

REFSTD = list(2, 1:5, 6:15)  #Two different reference standards ...
s2 = c(0.40, 0.6)	 #Sensitivity of the reference tests
c2 = c(0.75,0.95) 	 #Specificity of the reference tests	

#Thus, for the first 5 studies, S2 = 0.40 and C2 = 0.75 while for the last 
#10 studies s2 = 0.6 and c2 = 0.95


sim.data = simdata(N=N, n=seq(30,120,1), n.random=TRUE, sub_rs = REFSTD, 
   prev=pi,  se_ref=s2, sp_ref=c2, T=THETA, range.T=c(-5,5),L=LAMBDA, 
   sd_t=sd_theta,sd_a=sd_alpha, b=beta)
}

\keyword{datagen}

