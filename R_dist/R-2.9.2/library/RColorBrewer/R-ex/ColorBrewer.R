### Name: RColorBrewer
### Title: ColorBrewer palettes
### Aliases: brewer.pal display.brewer.pal display.brewer.all
###   brewer.pal.info brewer colors ColorBrewer RColorBrewer brewer.all
### Keywords: color

### ** Examples

## create a sequential palette for usage and show colors
mypalette<-brewer.pal(7,"Greens")
image(1:7,1,as.matrix(1:7),col=mypalette,xlab="Greens (sequential)",
       ylab="",xaxt="n",yaxt="n",bty="n")
Sys.sleep(2)
## display a divergent palette
display.brewer.pal(7,"BrBG")
Sys.sleep(2)
## display a qualitative palette
display.brewer.pal(7,"Accent")
Sys.sleep(2)
## display a palettes simultanoeusly
display.brewer.all(n=10, exact.n=FALSE)
Sys.sleep(2)
display.brewer.all(n=10)
Sys.sleep(2)
display.brewer.all()
Sys.sleep(2)
display.brewer.all(type="div")
Sys.sleep(2)
display.brewer.all(type="seq")
Sys.sleep(2)
display.brewer.all(type="qual") 
Sys.sleep(2)
display.brewer.all(n=5,type="div",exact.n=TRUE)
Sys.sleep(2)
brewer.pal.info
brewer.pal.info["Blues",]
brewer.pal.info["Blues",]$maxcolors



