### runRUnitTests.R
###------------------------------------------------------------------------
### What: Run RUnit tests (the core)- R code
### $Id$
### Time-stamp: <2008-12-30 12:52:51 ggorjan>
###------------------------------------------------------------------------

## The setup seems to be quite messy, but it is so to enable use of this in
## several ways as shown bellow.

## "R CMD check" way should be the most authoritative way to run the RUnit
## tests for a developer. RUnit tests are issued during R CMD check of the
## package due to example section of .runRUnitTests() function. If any test
## fails (failure) or if there are any R errors during RUnit testing, R CMD
## check fails. These are variable values specific for this way:
##  - .path DEVEL/PATH/PKG.Rcheck/PKG/unitTests
##  - .way  function

## ".runRUnitTests()" way from within R after library(PKG) is handy for
## package useRs, since it enables useRs to be sure that all tests pass for
## their installation. This is just a convenient wrapper function to run
## the RUnit testing suite. These are variable values specific for this
## way:
##  - .path INSTALL/PATH/PKG/unitTests
##  - .way  function

## "Shell" way is another possibility mainly for a developer in order to
## skip possibly lengthy R CMD check and perform just RUnit testing with an
## installed version of a pcakage. These are variable values specific for
## this way:
##  - .path DEVEL/PATH/PKG/inst/unitTests
##  - .way  shell
##
## Rscript runRUnitTests.R
## R CMD BATCH runRUnitTests.R
## make
## make all

## Sourced via shell (Makefile, Rscript, R CMD BATCH)
if(!exists(".pkg")) {
  .path <- getwd()
  .way <- "shell"
  .pkg <- c(read.dcf(file="../../DESCRIPTION", fields="Package"))
  print(.pkg)
  testFileRegexp <- "^base.+\\.[rR]$"
}

if(require("RUnit", quietly=TRUE)) {

  ## Debugging echo
  cat("\nRunning RUnit tests\n")
  print(list(pkg=.pkg, getwd=getwd(), pathToRUnitTests=.path))

  ## Load the package - not needed for .runRUnitTests()
  if(.way %in% c("shell"))
    library(package=.pkg, character.only=TRUE)

  ## Define tests
  testSuite <- defineTestSuite(name=paste(.pkg, "RUnit testing"),
                               dirs=.path, testFileRegexp=testFileRegexp)

  ## Run
  tests <- runTestSuite(testSuite)

  if(file.access(.path, 02) != 0) {
    ## cannot write to .path -> use writable one
    tdir <- tempfile(paste(.pkg, "RUnitTests", sep="_"))
    dir.create(tdir)
    pathReport <- file.path(tdir, "report")
  } else {
    pathReport <- file.path(.path, "report")
  }

  ## Print results:
  printTextProtocol(tests)
  printTextProtocol(tests,
                    fileName=paste(pathReport, ".txt", sep=""))

  ## Print HTML Version of results:
  printHTMLProtocol(tests,
                    fileName=paste(pathReport, ".html", sep=""))

  cat("\nRUnit reports also written to\n",
      pathReport, ".(txt|html)\n\n", sep="")

  ## Return stop() to cause R CMD check stop in case of
  ##  - failures i.e. FALSE to RUnit tests or
  ##  - errors i.e. R errors
  tmp <- getErrors(tests)
  if(tmp$nFail > 0 || tmp$nErr > 0) {
    stop(paste("\n\nRUnit testing failed:\n",
               " - #test failures: ", tmp$nFail, "\n",
               " - #R errors: ",  tmp$nErr, "\n\n", sep=""))
  }

} else {

  cat("R package 'RUnit' cannot be loaded - no unit tests run\n",
      "for package", .pkg,"\n")

}

###------------------------------------------------------------------------
### runRUnitTests.R ends here
