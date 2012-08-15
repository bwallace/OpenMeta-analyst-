\name{MRI}
\alias{MRI}
\docType{data}
\title{ MAGNETIC RESONANCE IMAGING TEST (MRI) for evaluation of lymph node metastasis in women with invasive cervical cancer }

\description{
This dataset gives the observed cross-tabulation of the MRI (test under evaluation) and histologic/cytologic specimens obtained by surgery or lymph node biopsy (reference test).
}

\usage{data(MRI)}

\format{
  A matrix with 10 observations on the following 4 variables.
  \describe{
    \item{\code{++}}{Observed individuals who tested positive on both tests }
    \item{\code{+-}}{Observed individuals who tested positive on the test under evaluation and negative on the reference test}
    \item{\code{-+}}{Observed individuals who tested negative on the test under evaluation and positive on the reference test}
    \item{\code{---}}{Observed individuals who tested negative on both tests }
  }
}



\references{ Scheidler J, Hricak H, Yu KK, Subak L, Segal MR. \emph{Radiological evaluation of lymph node metastases in patients with cervical cander : a meta-analysis}.
Journal of the American Medical Association 1997 ; 278(13):1096-1101. 

C. M. Rutter and C. A. Gatsonis. \emph{A hierarchical regression approach to meta-analysis of diagnostic accuracy evaluations}. Statistics in Medicine 2001 ; 20(19):2865-2884.

}
\examples{
data(MRI)
MRI

}
\keyword{datasets}
