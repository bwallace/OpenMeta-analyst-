\name{reg.data}
\alias{reg.data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(reg.data)}
\format{
  The format is:
List of 7
 $ label              : chr [1:25] "study3" "subgroup1" "study3" "study4" ...
 $ types              : num [1:25] 0 0 0 0 1 0 0 0 1 0 ...
 $ scale              : chr "log"
 $ additional.col.data:List of 1
  ..$ col1: chr [1:25] "study3" "subgroup1" "study3" "study4" ...
 $ effects            :List of 3
  ..$ ES: num [1:25] 0.0953 0.0953 0.0953 0.0953 0.0953 ...
  ..$ LL: num [1:25] -0.223 -0.223 -0.223 -0.223 -0.223 ...
  ..$ UL: num [1:25] 0.336 0.336 0.336 0.336 0.336 ...
 $ covariate          :List of 2
  ..$ varname: chr "lala"
  ..$ values : num [1:18] -2 0 1.17 2.97 1.86 1.05 1.27 1.17 1.17 2.97 ...
 $ fitted.line        :List of 2
  ..$ intercept: num -1
  ..$ slope    : num 1.7
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(reg.data)
## maybe str(reg.data) ; plot(reg.data) ...
}
\keyword{datasets}
