\name{binary.fixed.mh}
\alias{binary.fixed.mh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binary Fixed Mantel-Haenszel}
\description{Function to perform a binary fixed-effect meta-analysis, using the Mantel-Haenszel method.
}
\usage{
binary.fixed.mh(binary.data, params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binary.data}{Data set of class BinaryData}
  \item{params}{List of parameters for the function}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
binary.data <- new('BinaryData', g1O1=c(4,6,3,62,33,180,8), g1O2=c(123,306,231,13598,5069,1541,2545), 
                       g2O1=c(11,29,11,248,47,372,10), g2O2=c(139,303,220,12867,5808,1451,629), 
                       study.names=c('Aaronson', 'Ferguson', 'Rosenthal', 'Hart', 'Frimodt-Moller', 'Stein', 'Vandiviere'))
params <- params <- list(measure="OR", conf.level=95, digits=3)
# More parameters:

params$create.plot <- TRUE   
# When TRUE, the function creates a forest plot of the data and saves it to the directory specified by params$fp_outpath. 
# In this case, you must also specify the parameters below. If parms$create.plot <- FALSE, no plot is created and the parameters below are not
# required.
           
params$fp_outpath <- "./plotname.png"    
# Saves the forest plot as plotname.png in the current working directory for R. To save in a different directory, 
# enter the full path to the directory and file name. For example, params$fp_outpath <- "C;/r_tmp/plotname.png" 
                                         
params$fp_show_col2 <- TRUE 
# When TRUE, the function displays a column of effects sizes and confidence bounds for each study, 
# and for the overall results, in the forest plot.

params$fp_col2_str <- "ES (LL, UL)"
# Label for the column of effect sizes and confidence bounds. Not required if params$fp_show_col2 <- FALSE

params$fp_show_col3 <- TRUE
# When TRUE, the forest plot displays the following data in column 3:  binary.data@g1O1, ' / ', binary.data@g1O1 + binary.data@g1O2

params$fp_col3_str <- "Ev / Trt"
# Label for column 3. Not required if params$fp_show_col3 <- FALSE

params$fp_show_col4 <- TRUE    
# When TRUE, the forest plot displays the following data in column 4:  binary.data@g2O1, ' / ', binary.data@g2O1 + binary.data@g2O2   

params$fp_col4_str <- "Ev / Ctrl"
# Label for column 4. Not required if params$fp_show_col4 <- FALSE   

results <- binary.fixed.mh(binary.data, params)          
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
