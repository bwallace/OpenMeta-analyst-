### Name: read.ssd
### Title: Obtain a Data Frame from a SAS Permanent Dataset, via read.xport
### Aliases: read.ssd
### Keywords: file

### ** Examples

## if there were some files on the web we could get a real
## runnable example
## Not run: 
##D R> list.files("trialdata")
##D  [1] "baseline.sas7bdat" "form11.sas7bdat"   "form12.sas7bdat"  
##D  [4] "form13.sas7bdat"   "form22.sas7bdat"   "form23.sas7bdat"  
##D  [7] "form3.sas7bdat"    "form4.sas7bdat"    "form48.sas7bdat"  
##D [10] "form50.sas7bdat"   "form51.sas7bdat"   "form71.sas7bdat"  
##D [13] "form72.sas7bdat"   "form8.sas7bdat"    "form9.sas7bdat"   
##D [16] "form90.sas7bdat"   "form91.sas7bdat"  
##D R> baseline <- read.ssd("trialdata","baseline")
##D R> form90 <- read.ssd("trialdata","form90")
##D 
##D ## Or for a Windows example
##D sashome <- "/Program Files/SAS/SAS 9.1"
##D read.ssd(file.path(sashome, "core", "sashelp"), "retail",
##D          sascmd = file.path(sashome, "sas.exe"))
## End(Not run)



