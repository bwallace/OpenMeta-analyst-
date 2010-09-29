### Name: Cairo
### Title: Create a new Cairo-based graphics device
### Aliases: Cairo CairoX11 CairoPNG CairoPDF CairoSVG CairoPS CairoWin
###   CairoJPEG CairoTIFF
### Keywords: device

### ** Examples

# very simple KDE
Cairo(600, 600, file="plot.png", type="png", bg="white")
plot(rnorm(4000),rnorm(4000),col="#ff000018",pch=19,cex=2) # semi-transparent red
dev.off() # creates a file "plot.png" with the above plot

# you can use any Cairo backend and get the same result
# vector, bitmap or on-screen
CairoPDF("plot.pdf", 6, 6, bg="transparent")
data(iris)
attach(iris)
plot(Petal.Length, rep(-0.03,length(Species)), xlim=c(1,7),
     ylim=c(0,1.7), xlab="Petal.Length", ylab="Density",
     pch=21, cex=1.5, col="#00000001", main = "Iris (yet again)",
     bg=c("#ff000020","#00ff0020","#0000ff20")[unclass(Species)])
for (i in 1:3)
  polygon(density(Petal.Length[unclass(Species)==i],bw=0.2),
    col=c("#ff000040","#00ff0040","#0000ff40")[i])
dev.off()



