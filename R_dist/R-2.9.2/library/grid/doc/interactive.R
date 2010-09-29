###################################################
### chunk number 1: 
###################################################
library(grDevices)
library(grid)
ps.options(pointsize=12)
options(width=60)



###################################################
### chunk number 2: fig1
###################################################
grid.xaxis(at=1:4/5, vp=viewport(w=.5, h=0.01), name="gxa")



###################################################
### chunk number 3: edit1
###################################################
grid.edit("gxa", gp=gpar(col="red"))



###################################################
### chunk number 4: fig2
###################################################
gxa <- xaxisGrob(at=1:4/5, vp=viewport(w=.5, h=.01))
gxa <- editGrob(gxa, gp=gpar(col="red"))
grid.draw(gxa)



###################################################
### chunk number 5: edit2
###################################################
grid.edit(gPath("gxa", "labels"), gp=gpar(col="green"))



###################################################
### chunk number 6: fig3
###################################################
gxa <- xaxisGrob(at=1:4/5, vp=viewport(w=.5, h=.01))
gxa <- editGrob(gxa, gp=gpar(col="red"))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="green"))
grid.draw(gxa)



###################################################
### chunk number 7: edit3
###################################################
grid.edit("gxa", at=c(0.0, 0.5, 1.0))



###################################################
### chunk number 8: fig4
###################################################
gxa <- xaxisGrob(at=1:4/5, vp=viewport(w=.5, h=.01))
gxa <- editGrob(gxa, gp=gpar(col="red"))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="green"))
gxa <- editGrob(gxa, at=c(0.0, 0.5, 1.0))
grid.draw(gxa)



###################################################
### chunk number 9: edit4
###################################################
grid.edit("gxa::labels", gp=gpar(col="black"), rot=30)



###################################################
### chunk number 10: fig5
###################################################
gxa <- xaxisGrob(at=1:4/5, vp=viewport(w=.5, h=.01))
gxa <- editGrob(gxa, gp=gpar(col="red"))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="green"))
gxa <- editGrob(gxa, at=c(0.0, 0.5, 1.0))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="black"), rot=30)
grid.draw(gxa)



###################################################
### chunk number 11: 
###################################################
gxa <- xaxisGrob(at=1:4/5, vp=viewport(w=.5, h=.01))
gxa <- editGrob(gxa, gp=gpar(col="red"))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="green"))
gxa <- editGrob(gxa, at=c(0.0, 0.5, 1.0))
gxa <- editGrob(gxa, gPath="labels", gp=gpar(col="black"), rot=30)
grid.draw(gxa)



###################################################
### chunk number 12:  eval=FALSE
###################################################
## grid.newpage()
## 


