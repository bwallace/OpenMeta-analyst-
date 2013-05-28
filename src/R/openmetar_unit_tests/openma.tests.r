
library(openmetar)

test.all <- function() {
  try.errors <- array(list())
  
  params <- set.params(data.type="binary")
  binary.data <- create.binary.data(params)
  try.errors <- test.binary.functions(binary.data, params, try.errors)
  
  cont.data <- create.cont.data(params)
  try.errors <- test.cont.functions(cont.data, params, try.errors)
  
  params <- set.params(data.type="diagnostic")
  diagnostic.data <- create.diag.data(params)
  try.errors <- test.diag.functions(diagnostic.data, params, try.errors)
  try.errors
}  

create.binary.data <- function(params) {
  ai<- c(4,6,3,62,33,180,8,505)
  n1i<-c(123,306,231,13598,5069,1541,2545,88391)
  bi<-n1i - ai
  ci<-c(11,29,11,248,47,372,10,499)
  n2i<-c(139,303,220,12867,5808,1451,629,88391)
  di<-n2i-ci
  binary.data <- new('BinaryData', g1O1=ai, g1O2=bi,
                       g2O1=ci, g2O2=di,
                       study.names=c('Aaronson', 'Ferguson', 'Rosenthal', 'Hart', 'Frimodt-Moller', 'Stein', 'Vandiviere', 'TPT Madras'),
                       covariates=list(latitude=c(44,55,42,52,13,44,19,13), groups=c('1','1','2','2','1','2','2','1')))
  
  res <- compute.for.one.bin.study(binary.data, params)    
  binary.data@y <- res$yi
  binary.data@SE <- sqrt(res$vi)
  binary.data
}

create.binary.fnames <- function() {
  binary.fnames <- c("binary.fixed.inv.var", "binary.fixed.mh", 
                     "binary.fixed.peto", "binary.random")
}  

create.binary.meta.fnames <- function() {
  # binary meta functions except for subgroup
  binary.meta.fnames <- c("cumul.ma.binary", "loo.ma.binary")
}

create.cont.data <- function(params) {
  N1 <- c(60,65,40,200,50,85)
  mean1 <- c(94,98,98,94,98,96)
  sd1 <- c(22,21,28,19,21,21)
  N2 <- c(60,65,40,200,45,85)
  mean2 <- c(92,92,88,82,88,92)
  sd2 <- c(20,22,26,17,22,22)
  cont.data <- new('ContinuousData', N1=N1, mean1=mean1, sd1=sd1,
                      N2=N2, mean2=mean2, sd2=sd2,
                      study.names=c("Carroll", "Grant", "Peck", "Donat", "Stewart", "Young"),
                      covariates=list(groups=c('1','1','2','1','2','2')))
  res <- compute.for.one.cont.study(cont.data, params)    
  cont.data@y <- res$yi
  cont.data@SE <- sqrt(res$vi)
  cont.data
}

create.cont.fnames <- function() {
  cont.fnames <- c("continuous.fixed", "continuous.random")
}

create.diag.data <- function(params) {
  diagnostic.data <- new('DiagnosticData', TP=c(81, 16, 8, 4, 15, 12, 1, 18, 18, 112, 14, 7, 57, 7, 6, 18, 2, 1, 37, 4), 
                                             FN=c(5, 0, 0, 0, 2, 3, 0, 0, 3, 2, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0),
                                             TN=c(186, 48, 14, 30, 15, 34, 16, 32, 13, 414, 33, 44, 26, 14, 24, 96, 18, 4, 91, 18), 
                                             FP=c(273, 90, 98, 11, 35, 51, 11, 125, 55, 601, 57, 127, 35, 38, 59, 35, 21, 9, 72, 32), 
                       study.names=c('Hmeidan', 'Auslender', 'Botsis','Cacclatore', 'Chan', 'Dorum', 'Goldstein', 'Granberg',
                                     'Hanggi', 'Karlsson (a)', 'Karlsson (b)', 'Klug', 'Malinova', 'Nasri (a)', 'Nasri (b)', 'Petrl', 'Taviani', 'Varner', 'Weigel', 'Wolman'))
   res <- get.res.for.one.diag.study(diagnostic.data,params)
   diagnostic.data@y <- res$b
   diagnostic.data@SE <- res$se 
   diagnostic.data 
}

create.diag.fnames <- function() {
  diag.fnames <- c("diagnostic.fixed", "diagnostic.random")
}

set.params <- function(data.type) {
  params <- list(conf.level=95, digits=3)
  params$fp_show_col2 <- TRUE
  params$fp_show_col3 <- TRUE
  params$fp_show_col4 <- TRUE
  params$fp_col1_str <- "Studies"
  params$fp_col2_str <- "ES (LL, UL)"
  params$fp_col3_str <- "Ev / Trt"
  params$fp_col4_str <- "Ev / Ctrl"
  params$fp_show_summary_line <- "TRUE"
  params$fp_outpath <- "./r_tmp/forest_plot.png"
  params$adjust <- 0.5
  params$to <- "only0"
  params$fp_xlabel <- "Effect size"
  params$rm.method <- "DL"
  if (data.type == "binary") {
    params$measure <- "OR"
    params$fp_show_col4 <- FALSE
  } else {
    params$measure <- "Sens"
    params$fp_show_col4 <- FALSE
  }
  params
}

test.binary.functions <- function(binary.data, params, try.errors) {
  binary.fnames <- create.binary.fnames()
  binary.meta.fnames <- create.binary.meta.fnames()
  # test standard functions
  for (fname in binary.fnames) {
    results <- call.function(fname, om.data=binary.data, params)
     if (class(results) == "try-error") {
       try.errors[[fname]] <- results
     }
     # test meta functions for each standard function
     
     for (meta.fname in binary.meta.fnames) {
       results <- call.meta.function(meta.fname, fname, om.data=binary.data, params)
       if (class(results) == "try-error") {
         try.errors[[meta.fname]] <- results
       }  
     }
     results <- subgroup.ma.binary(fname, binary.data, params, cov.name="groups")
     if (class(results) == "try-error") {
        try.errors[[meta.fname]] <- results
     }
  }
  try.errors
}

test.cont.functions <- function(cont.data, params, try.errors) {
   cont.fnames <- create.cont.fnames()
   for (fname in cont.fnames) {
     results <- call.function(fname, om.data=cont.data, params)
     if (class(results) == "try-error") {
       try.errors[[fname]] <- results
     }
  }
  try.errors
}

test.diag.functions <- function(diagnostic.data, params, try.errors) {
   diag.fnames <- create.diag.fnames()
   for (fname in diag.fnames) {
     results <- call.function(fname, om.data=diagnostic.data, params)
     if (class(results) == "try-error") {
       try.errors[[fname]] <- results
     }
  }
  try.errors
}

call.function <- function(fname, om.data, params) {
   results <- try(eval(call(fname, om.data, params)), silent=TRUE)
}

call.meta.function <- function(meta.fname, fname, om.data, params, cov.name) {
   results <- try(eval(call(meta.fname, fname, om.data, params)), silent=TRUE)
}