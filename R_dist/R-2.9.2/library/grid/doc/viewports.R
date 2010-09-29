###################################################
### chunk number 1: 
###################################################
library(grDevices)
library(stats) # for runif()
library(grid)
ps.options(pointsize=12)
options(width=60)



###################################################
### chunk number 2: 
###################################################
pushViewport(viewport())
upViewport()
pushViewport(viewport())



###################################################
### chunk number 3: 
###################################################
grid.newpage()



###################################################
### chunk number 4: 
###################################################
pushViewport(viewport(name="A"))
upViewport()
pushViewport(viewport(name="B"))
upViewport()
downViewport("A")



###################################################
### chunk number 5: 
###################################################
seekViewport("B")



###################################################
### chunk number 6: 
###################################################
current.vpTree()



###################################################
### chunk number 7: vpstackguts
###################################################
vp <- viewport(width=0.5, height=0.5)
grid.rect(vp=vpStack(vp, vp))



###################################################
### chunk number 8: 
###################################################
grid.rect(gp=gpar(col="grey"))
vp <- viewport(width=0.5, height=0.5)
grid.rect(vp=vpStack(vp, vp))




###################################################
### chunk number 9: 
###################################################
grid.newpage()



###################################################
### chunk number 10: 
###################################################
pushViewport(viewport(name="A"))
pushViewport(viewport(name="B"))
pushViewport(viewport(name="A"))



###################################################
### chunk number 11: 
###################################################
seekViewport("A")
current.vpTree(FALSE)



###################################################
### chunk number 12: 
###################################################
seekViewport(vpPath("B", "A"))
current.vpTree(FALSE)



###################################################
### chunk number 13: 
###################################################
vpPath("A", "B")



###################################################
### chunk number 14:  eval=FALSE
###################################################
## seekViewport(vpPath("A", "B"))
## seekViewport("A::B")
## 


###################################################
### chunk number 15: 
###################################################
x <- runif(10)
y <- runif(10)



###################################################
### chunk number 16: 
###################################################
xscale <- extendrange(x)
yscale <- extendrange(y)



###################################################
### chunk number 17: 
###################################################
top.vp <- viewport(layout=grid.layout(3, 3,
  widths=unit(c(5, 1, 2), c("lines", "null", "lines")),
  heights=unit(c(5, 1, 4), c("lines", "null", "lines"))))



###################################################
### chunk number 18: 
###################################################
grid.show.layout(viewport.layout(top.vp))



###################################################
### chunk number 19: 
###################################################
margin1 <- viewport(layout.pos.col=2, layout.pos.row=3,
  name="margin1")
margin2 <- viewport(layout.pos.col=1, layout.pos.row=2,
  name="margin2")
margin3 <- viewport(layout.pos.col=2, layout.pos.row=1,
  name="margin3")
margin4 <- viewport(layout.pos.col=3, layout.pos.row=2,
  name="margin4")
plot <- viewport(layout.pos.col=2, layout.pos.row=2,
  name="plot", xscale=xscale, yscale=yscale)



###################################################
### chunk number 20: 
###################################################
splot <- vpTree(top.vp, vpList(margin1, margin2, margin3, margin4, plot))



###################################################
### chunk number 21: viewports
###################################################
pushViewport(splot)



###################################################
### chunk number 22: grid eval=FALSE
###################################################
## labelvp <- function(name) {
##   seekViewport(name)
##   grid.rect(gp=gpar(col="grey", lwd=5))
##   grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
##     height=unit(1, "lines"), just=c("left", "top"),
##     gp=gpar(fill="grey", col=NULL))
##   grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
##     just=c("left", "top"), gp=gpar(col="white"))
## }
## labelvp("plot")
## labelvp("margin1")
## labelvp("margin2")
## labelvp("margin3")
## labelvp("margin4")
## 


###################################################
### chunk number 23: plot eval=FALSE
###################################################
## seekViewport("plot")
## grid.points(x, y)
## grid.xaxis()
## grid.yaxis()
## grid.rect()
## 


###################################################
### chunk number 24: margin1 eval=FALSE
###################################################
## seekViewport("margin1")
## grid.text("Random X", y=unit(1, "lines"))
## 


###################################################
### chunk number 25: margin2 eval=FALSE
###################################################
## seekViewport("margin2")
## grid.text("Random Y", x=unit(1, "lines"), rot=90)
## 


###################################################
### chunk number 26: 
###################################################
pushViewport(viewport(w=0.9, h=0.9))
pushViewport(splot)

labelvp <- function(name) {
  seekViewport(name)
  grid.rect(gp=gpar(col="grey", lwd=5))
  grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
    height=unit(1, "lines"), just=c("left", "top"),
    gp=gpar(fill="grey", col=NULL))
  grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
    just=c("left", "top"), gp=gpar(col="white"))
}
labelvp("plot")
labelvp("margin1")
labelvp("margin2")
labelvp("margin3")
labelvp("margin4")

seekViewport("plot")
grid.points(x, y)
grid.xaxis()
grid.yaxis()
grid.rect()

seekViewport("margin1")
grid.text("Random X", y=unit(1, "lines"))

seekViewport("margin2")
grid.text("Random Y", x=unit(1, "lines"), rot=90)




###################################################
### chunk number 27: 
###################################################
upViewport(0)



###################################################
### chunk number 28: annguts eval=FALSE
###################################################
## seekViewport("margin3")
## grid.text("The user adds a title!", gp=gpar(fontsize=20))
## 


###################################################
### chunk number 29: 
###################################################
pushViewport(viewport(w=0.9, h=0.9))
pushViewport(splot)

labelvp <- function(name) {
  seekViewport(name)
  grid.rect(gp=gpar(col="grey", lwd=5))
  grid.rect(x=0, y=1, width=unit(1, "strwidth", name) + unit(2, "mm"),
    height=unit(1, "lines"), just=c("left", "top"),
    gp=gpar(fill="grey", col=NULL))
  grid.text(name, x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
    just=c("left", "top"), gp=gpar(col="white"))
}
labelvp("plot")
labelvp("margin1")
labelvp("margin2")
labelvp("margin3")
labelvp("margin4")

seekViewport("plot")
grid.points(x, y)
grid.xaxis()
grid.yaxis()
grid.rect()

seekViewport("margin1")
grid.text("Random X", y=unit(1, "lines"))

seekViewport("margin2")
grid.text("Random Y", x=unit(1, "lines"), rot=90)

seekViewport("margin3")
grid.text("The user adds a title!", gp=gpar(fontsize=20))

popViewport(0)



###################################################
### chunk number 30:  eval=FALSE
###################################################
## grid.newpage()
## 


