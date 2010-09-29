###################################################
### chunk number 1: 
###################################################
library(grDevices)
library(graphics) # for plot()
library(stats) # for runif()
library(grid)
ps.options(pointsize=12)
options(width=60)



###################################################
### chunk number 2: guts1 eval=FALSE
###################################################
## plot(1:10)
## par(new=TRUE)
## grid.rect(width=0.5, height=0.5, gp=gpar(lwd=3), name="gr")
## 


###################################################
### chunk number 3: ex1
###################################################
plot(1:10)
par(new=TRUE)
grid.rect(width=0.5, height=0.5, gp=gpar(lwd=3), name="gr")

grid.rect(width=0.99, height=0.99, gp=gpar(lty="dashed"))



###################################################
### chunk number 4: ex2
###################################################
grid.rect(width=0.5, height=0.5, gp=gpar(col="red", lwd=3))
grid.rect(width=0.99, height=0.99, gp=gpar(lty="dashed"))



###################################################
### chunk number 5:  eval=FALSE
###################################################
## grid.edit("gr", gp=gpar(col="red", lwd=3))
## 


###################################################
### chunk number 6: 
###################################################
grid.rect(width=convertWidth(unit(1, "inches"), "npc"))



###################################################
### chunk number 7: 
###################################################
drawDetails.myrect <- function(x, recording) {
  gr <- rectGrob(width=convertWidth(unit(1, "inches"), "npc"))
  grid.draw(gr)
}
grid.draw(grob(cl="myrect"))




###################################################
### chunk number 8: 
###################################################
if ("gridBase" %in% .packages(all.available=TRUE)) {
library(gridBase)

x <- c(0.88, 1.00, 0.67, 0.34)
y <- c(0.87, 0.43, 0.04, 0.94)
z <- matrix(runif(4*2), ncol=2)

maxpiesize <- unit(1, "inches")
totals <- apply(z, 1, sum)
sizemult <- totals/max(totals)

gs <- segmentsGrob(x0=unit(c(rep(0, 4), x),
                        rep(c("npc", "native"), each=4)),
                x1=unit(c(x, x), rep("native", 8)),
                y0=unit(c(y, rep(0, 4)),
                        rep(c("native", "npc"), each=4)),
                y1=unit(c(y, y), rep("native", 8)),
                gp=gpar(lty="dashed", col="grey"))
gr <- rectGrob(gp=gpar(col="grey", fill="white", lty="dashed"))
}



###################################################
### chunk number 9: 
###################################################
drawDetails.pieplot <- function(x, recording) {
  plot(x$x, x$y, xlim=c(-0.2, 1.2), ylim=c(-0.2, 1.2), type="n")
  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot, recording=FALSE)
  grid.draw(x$gs, recording=FALSE)
  for (i in 1:4) {
    pushViewport(viewport(x=unit(x$x[i], "native"),
                           y=unit(x$y[i], "native"),
                           width=x$sizemult[i]*x$maxpiesize,
                           height=x$sizemult[i]*x$maxpiesize),
                 recording=FALSE)
    grid.draw(x$gr, recording=FALSE)
    par(plt=gridPLT(), new=TRUE)
    pie(x$z[i,], radius=1, labels=rep("", 2))
    popViewport(recording=FALSE)
  }
  popViewport(3, recording=FALSE)
}



###################################################
### chunk number 10: 
###################################################
if ("gridBase" %in% .packages()) {
grid.draw(grob(x=x, y=y, z=z,
               maxpiesize=maxpiesize, sizemult=sizemult,
               gs=gs, gr=gr, cl="pieplot"))
}



###################################################
### chunk number 11: 
###################################################
drawDetails.mylegend <- function(x, recording) {
   x <- 0:64/64
   y <- sin(3*pi*x)
   plot(x, y, type="l", col="blue", main = "points with bg & legend(*, pt.bg)")
   points(x, y, pch=21, bg="white")
   legend(.4,1, "sin(c x)", pch=21, pt.bg="white", lty=1, col = "blue")
}
grid.draw(grob(cl="mylegend"))




###################################################
### chunk number 12:  eval=FALSE
###################################################
## grid.newpage()
## 


