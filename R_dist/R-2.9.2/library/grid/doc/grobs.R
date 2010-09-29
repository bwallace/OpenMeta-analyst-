###################################################
### chunk number 1: 
###################################################
library(grDevices)
library(grid)
ps.options(pointsize=12)
options(width=60)



###################################################
### chunk number 2: 
###################################################
gl <- linesGrob()
gl <- editGrob(gl, gp=gpar(col="green"))



###################################################
### chunk number 3: 
###################################################
grid.newpage()
grid.lines(name="lines")
grid.edit(gPath("lines"), gp=gpar(col="pink"))
grid.edit("lines", gp=gpar(col="red"))



###################################################
### chunk number 4: 
###################################################
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
grid.rect(gp=gpar(col="grey"))
grid.xaxis(name="myxaxis")
grid.edit("myxaxis", at=1:4/5)
grid.edit(gPath("myxaxis", "labels"), y=unit(-1, "lines"))



###################################################
### chunk number 5: 
###################################################
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
myplot <- gTree(name="myplot",
                children=gList(
                  rectGrob(name="box", gp=gpar(col="grey")),
                  xaxisGrob(name="xaxis")))
grid.draw(myplot)
grid.edit("myplot::xaxis", at=1:10/11)
grid.edit("myplot::xaxis::labels", label=round(1:10/11, 2))
grid.edit("myplot::xaxis::labels", y=unit(-1, "lines"))



###################################################
### chunk number 6: 
###################################################
grid.newpage()
gt <- grid.text("Hi there")
grid.rect(width=unit(1, "grobwidth", gt))



###################################################
### chunk number 7: 
###################################################
grid.newpage()
gt <- grid.text("Hi there", name="sometext")
grid.rect(width=unit(1, "grobwidth", "sometext"))
grid.edit("sometext", label="Something different")



###################################################
### chunk number 8: 
###################################################
grid.newpage()
mygrob <- grob(name="mygrob", cl="mygrob")
preDrawDetails.mygrob <- function(x) {
  pushViewport(viewport(gp=gpar(fontsize=20)))
}
drawDetails.mygrob <- function(x, recording=TRUE) {
  grid.draw(textGrob("hi there"), recording=FALSE)
}
postDrawDetails.mygrob <- function(x) {
  popViewport()
}
widthDetails.mygrob <- function(x) {
  unit(1, "strwidth", "hi there")
}
grid.draw(mygrob)
grid.rect(width=unit(1, "grobwidth", mygrob))



###################################################
### chunk number 9: 
###################################################
grid.newpage()
mygtree <- gTree(name="mygrob",
                 childrenvp=viewport(name="labelvp", gp=gpar(fontsize=20)),
                 children=gList(textGrob("hi there", name="label",
                   vp="labelvp")),
                 cl="mygtree")
widthDetails.mygtree <- function(x) {
  unit(1, "grobwidth", getGrob(x, "label"))
}
grid.draw(mygtree)
grid.rect(width=unit(1, "grobwidth", mygtree))



###################################################
### chunk number 10: 
###################################################
grid.newpage()
fg <- frameGrob(layout=grid.layout(1, 2))
fg <- placeGrob(fg, textGrob("Hi there"), col=1)
fg <- placeGrob(fg, rectGrob(), col=2)
grid.draw(fg)



###################################################
### chunk number 11: 
###################################################
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
drawIt <- function(row, col) {
  pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
  grid.rect(gp=gpar(col="grey"))
  grid.draw(fg)
  upViewport()
}
fg <- frameGrob()
fg <- packGrob(fg, textGrob("Hi there"))
fg <- placeGrob(fg, rectGrob())
drawIt(1, 1)
fg <- packGrob(fg, textGrob("Hello again"), side="right")
drawIt(1, 2)
fg <- packGrob(fg, rectGrob(), side="right", width=unit(1, "null"))
drawIt(2, 2)



###################################################
### chunk number 12: 
###################################################
grid.newpage()
fg <- frameGrob()
fg <- packGrob(fg, textGrob("Hi there"))
fg <- placeGrob(fg, rectGrob())
fg <- packGrob(fg, textGrob("Hello again", name="midtext"),
               side="right", dynamic=TRUE)
fg <- packGrob(fg, rectGrob(), side="right", width=unit(1, "null"))
grid.draw(fg)
grid.edit("midtext", label="something much longer")



###################################################
### chunk number 13: 
###################################################
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
drawIt <- function(row, col) {
  pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
  grid.rect(gp=gpar(col="grey"))
  grid.draw(gplot)
  upViewport()
}
gplot <- gTree(x=NULL, y=NULL,
               childrenvp=vpTree(
                 plotViewport(c(5, 4, 4, 2), name="plotRegion"),
                 vpList(viewport(name="dataRegion"))),
               children=gList(
                 xaxisGrob(vp="plotRegion::dataRegion"),
                 yaxisGrob(vp="plotRegion::dataRegion"),
                 rectGrob(vp="plotRegion")))
drawIt(1, 1)
gplot <- addGrob(gplot, pointsGrob(vp="plotRegion::dataRegion"))
drawIt(1, 2)
gplot <- addGrob(gplot, pointsGrob(name="data1", pch=2,
                                   vp="plotRegion::dataRegion"))
drawIt(2, 1)
gplot <- removeGrob(gplot, "data1")
drawIt(2, 2)



###################################################
### chunk number 14: 
###################################################
gplot <- gTree(x=NULL, y=NULL,
               childrenvp=vpTree(
                 plotViewport(c(5, 4, 4, 2), name="plotRegion"),
                   vpList(viewport(name="dataRegion"))),
               children=gList(
                 xaxisGrob(vp="plotRegion::dataRegion"),
                 yaxisGrob(vp="plotRegion::dataRegion"),
                 rectGrob(vp="plotRegion")))
save(gplot, file="gplot1")
gplot <- addGrob(gplot, pointsGrob(vp="plotRegion::dataRegion"))
save(gplot, file="gplot2")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col=1))
load("gplot1")
grid.draw(gplot)
popViewport()
pushViewport(viewport(layout.pos.col=2))
load("gplot2")
grid.draw(gplot)
popViewport()



###################################################
### chunk number 15: 
###################################################
myplot <- gTree(name="myplot",
                children=gList(
                  rectGrob(name="box", gp=gpar(col="grey")),
                  xaxisGrob(name="xaxis")))
myplot <- editGrob(myplot, gPath="xaxis", at=1:10/11)
myplot <- editGrob(myplot, gPath="xaxis::labels", label=round(1:10/11, 2))
myplot <- editGrob(myplot, gPath="xaxis::labels", y=unit(-1, "lines"))
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
grid.draw(myplot)



###################################################
### chunk number 16: 
###################################################
myplot <- gTree(name="myplot",
                children=gList(rectGrob(name="box", gp=gpar(col="grey")),
                  xaxisGrob(name="xaxis")))
getGrob(myplot, "xaxis")
myplot <- editGrob(myplot, gPath="xaxis", at=1:10/11)
getGrob(myplot, "xaxis::labels")
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
grid.draw(myplot)
grid.get("myplot")
grid.get("myplot::xaxis")
grid.get("myplot::xaxis::labels")



###################################################
### chunk number 17: 
###################################################
myplot <- gTree(name="myplot",
                children=gList(rectGrob(name="box", gp=gpar(col="grey")),
                  xaxisGrob(name="xaxis")))
myplot <- setGrob(myplot, "xaxis", rectGrob(name="xaxis"))
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
grid.draw(myplot)
grid.set("myplot::xaxis", xaxisGrob(name="xaxis", at=1:3/4))
grid.set("myplot::xaxis::labels",
         textGrob(name="labels", x=unit(1:3/4, "native"),
                  y=unit(-1, "lines"), label=letters[1:3]))
myplot <- setGrob(grid.get("myplot"), "xaxis::labels",
                  circleGrob(name="labels"))
grid.newpage()
pushViewport(viewport(w=.5, h=.5))
grid.draw(myplot)



###################################################
### chunk number 18: 
###################################################
drawIt <- function(row, col) {
  pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
  grid.rect(gp=gpar(col="grey"))
  grid.draw(gplot)
  upViewport()
}
gplot <- gTree(name="plot1",
               childrenvp=vpTree(
                 plotViewport(c(5, 4, 4, 2), name="plotRegion"),
                 vpList(viewport(name="dataRegion"))),
               children=gList(
                 xaxisGrob(name="xaxis", vp="plotRegion::dataRegion"),
                 yaxisGrob(name="yaxis", vp="plotRegion::dataRegion"),
                 rectGrob(name="box", vp="plotRegion")))
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
drawIt(1, 1)
grid.add("plot1", pointsGrob(0.5, 0.5, name="data1",
                             vp="plotRegion::dataRegion"))
grid.add("plot1::xaxis", textGrob("X Axis", y=unit(-2, "lines"), name="xlab"))
grid.edit("plot1::xaxis::xlab", y=unit(-3, "lines"))
gplot <- grid.get("plot1")
gplot <- addGrob(gplot, gPath="yaxis",
                 textGrob("Y Axis", x=unit(-3, "lines"), rot=90, name="ylab"))
drawIt(1, 2)
gplot <- removeGrob(gplot, "xaxis::xlab")
drawIt(2, 1)
grid.remove("plot1::data1")
grid.remove("plot1")



###################################################
### chunk number 19: 
###################################################
grid.newpage()
grid.frame(name="myframe", layout=grid.layout(1, 2))
grid.place("myframe", textGrob("Hi there"), col=1)
grid.place("myframe", rectGrob(), col=2)
grid.newpage()
grid.frame(name="frame2")
grid.pack("frame2", textGrob("Hi there"))
grid.place("frame2", rectGrob())
grid.pack("frame2", textGrob("Hello again"), side="right")
grid.pack("frame2", rectGrob(), side="right", width=unit(1, "null"))



###################################################
### chunk number 20:  eval=FALSE
###################################################
## grid.newpage()
## 


