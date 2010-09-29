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
x <- runif(10)
y <- runif(10)



###################################################
### chunk number 3: datavp
###################################################
data.vp <- viewport(x=unit(5, "lines"),
                    y=unit(4, "lines"),
                    width=unit(1, "npc") - unit(7, "lines"),
                    height=unit(1, "npc") - unit(7, "lines"),
                    just=c("left", "bottom"),
                    xscale=range(x) + c(-.05, .05)*diff(range(x)),
                    yscale=range(y) + c(-.05, .05)*diff(range(y)))



###################################################
### chunk number 4: procplot
###################################################
pushViewport(data.vp)
grid.points(x, y)
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("x axis", y=unit(-3, "lines"),
          gp=gpar(fontsize=14))
grid.text("y axis", x=unit(-4, "lines"),
          gp=gpar(fontsize=14), rot=90)
grid.text("A Simple Plot",
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16))
popViewport()



###################################################
### chunk number 5: 
###################################################
pushViewport(data.vp)
grid.points(x, y)
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("x axis", y=unit(-3, "lines"),
          gp=gpar(fontsize=14))
grid.text("y axis", x=unit(-4, "lines"),
          gp=gpar(fontsize=14), rot=90)
grid.text("A Simple Plot",
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16))
popViewport()




###################################################
### chunk number 6: ann1
###################################################
pushViewport(data.vp)
grid.text(date(), x=unit(1, "npc"),
  y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
popViewport()



###################################################
### chunk number 7: 
###################################################
pushViewport(data.vp)
grid.points(x, y)
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("x axis", y=unit(-3, "lines"),
          gp=gpar(fontsize=14))
grid.text("y axis", x=unit(-4, "lines"),
          gp=gpar(fontsize=14), rot=90)
grid.text("A Simple Plot",
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16))
popViewport()

pushViewport(data.vp)
grid.text(date(), x=unit(1, "npc"),
  y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
popViewport()




###################################################
### chunk number 8: 
###################################################
data.vp <- viewport(name="dataregion",
                    x=unit(5, "lines"),
                    y=unit(4, "lines"),
                    width=unit(1, "npc") - unit(7, "lines"),
                    height=unit(1, "npc") - unit(7, "lines"),
                    just=c("left", "bottom"),
                    xscale=range(x) + c(-.05, .05)*diff(range(x)),
                    yscale=range(y) + c(-.05, .05)*diff(range(y)))
pushViewport(data.vp)
grid.points(x, y)
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("x axis", y=unit(-3, "lines"),
          gp=gpar(fontsize=14))
grid.text("y axis", x=unit(-4, "lines"),
          gp=gpar(fontsize=14), rot=90)
grid.text("A Simple Plot",
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16))
upViewport()



###################################################
### chunk number 9: 
###################################################
downViewport("dataregion")
grid.text(date(), x=unit(1, "npc"),
  y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
upViewport()



###################################################
### chunk number 10: funcplot
###################################################
splot <- function(x=runif(10), y=runif(10), title="A Simple Plot") {
data.vp <- viewport(name="dataregion",
                    x=unit(5, "lines"),
                    y=unit(4, "lines"),
                    width=unit(1, "npc") - unit(7, "lines"),
                    height=unit(1, "npc") - unit(7, "lines"),
                    just=c("left", "bottom"),
                    xscale=range(x) + c(-.05, .05)*diff(range(x)),
                    yscale=range(y) + c(-.05, .05)*diff(range(y)))
pushViewport(data.vp)
grid.points(x, y)
grid.rect()
grid.xaxis()
grid.yaxis()
grid.text("y axis", x=unit(-4, "lines"),
          gp=gpar(fontsize=14), rot=90)
grid.text(title,
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16))
upViewport()
}



###################################################
### chunk number 11: embed
###################################################
grid.rect(gp=gpar(fill="grey"))
message <- paste("I could draw all sorts",
  "of stuff over here",
  "then create a viewport",
  "over there and stick",
  "a scatterplot in it.", sep="\n")
grid.text(message, x=0.25)
grid.lines(x=unit.c(unit(0.25, "npc") + 0.5*stringWidth(message) +
  unit(2, "mm"),
  unit(0.5, "npc") - unit(2, "mm")),
  y=0.5,
  arrow=arrow(angle=15, type="closed"),
  gp=gpar(lwd=3, fill="black"))
pushViewport(viewport(x=0.5, height=0.5, width=0.45, just="left",
  gp=gpar(cex=0.5)))
grid.rect(gp=gpar(fill="white"))
splot(1:10, 1:10, title="An Embedded Plot")
upViewport()



###################################################
### chunk number 12: ann2 eval=FALSE
###################################################
## downViewport("dataregion")
## grid.text(date(), x=unit(1, "npc"),
##   y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
## upViewport(0)
## 


###################################################
### chunk number 13: 
###################################################
grid.rect(gp=gpar(fill="grey"))
message <- paste("I could draw all sorts",
  "of stuff over here",
  "then create a viewport",
  "over there and stick",
  "a scatterplot in it.", sep="\n")
grid.text(message, x=0.25)
grid.lines(x=unit.c(unit(0.25, "npc") + 0.5*stringWidth(message) +
  unit(2, "mm"),
  unit(0.5, "npc") - unit(2, "mm")),
  y=0.5,
  arrow=arrow(angle=15, type="closed"),
  gp=gpar(lwd=3, fill="black"))
pushViewport(viewport(x=0.5, height=0.5, width=0.45, just="left",
  gp=gpar(cex=0.5)))
grid.rect(gp=gpar(fill="white"))
splot(1:10, 1:10, title="An Embedded Plot")
upViewport()

downViewport("dataregion")
grid.text(date(), x=unit(1, "npc"),
  y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
upViewport(0)




###################################################
### chunk number 14: 
###################################################
splot.data.vp <- function(x, y) {
  viewport(name="dataregion",
                    x=unit(5, "lines"),
                    y=unit(4, "lines"),
                    width=unit(1, "npc") - unit(7, "lines"),
                    height=unit(1, "npc") - unit(7, "lines"),
                    just=c("left", "bottom"),
                    xscale=range(x) + c(-.05, .05)*diff(range(x)),
                    yscale=range(y) + c(-.05, .05)*diff(range(y)))
}

splot.title <- function(title) {
      textGrob(title, name="title",
          y=unit(1, "npc") + unit(1.5, "lines"),
          gp=gpar(fontsize=16), vp="dataregion")
}

splot <- function(x, y, title, name=NULL, draw=TRUE, gp=gpar(), vp=NULL) {
  spg <- gTree(
    x=x, y=y, title=title,
    name=name,
    childrenvp = splot.data.vp(x, y),
    children=gList(rectGrob(name="border", vp="dataregion"),
      xaxisGrob(name="xaxis", vp="dataregion"),
      yaxisGrob(name="yaxis", vp="dataregion"),
      pointsGrob(x, y, name="points", vp="dataregion"),
      textGrob("x axis", y=unit(-3, "lines"), name="xlab",
          gp=gpar(fontsize=14), vp="dataregion"),
      textGrob("y axis", x=unit(-4, "lines"), name="ylab",
          gp=gpar(fontsize=14), rot=90, vp="dataregion"),
      splot.title(title)),
    gp=gp, vp=vp,
    cl="splot")
  if (draw)
    grid.draw(spg)
  spg
}



###################################################
### chunk number 15: splotgrob eval=FALSE
###################################################
## sg <- splot(1:10, 1:10, "Same as Before", name="splot", draw=FALSE)
## 


###################################################
### chunk number 16: 
###################################################
splot(1:10, 1:10, "Same as Before", name="splot")
downViewport("dataregion")
grid.text(date(), x=unit(1, "npc"),
  y = 0, just=c("right", "bottom"), gp=gpar(col="grey"))
upViewport(0)



###################################################
### chunk number 17: 
###################################################
splot(1:10, 1:10, "Same as Before", name="splot")
grid.edit("splot", gp=gpar(cex=0.5))



###################################################
### chunk number 18: 
###################################################
sg <- splot(1:10, 1:10, "Same as Before", name="splot", draw=FALSE)

sg <- editGrob(sg, gp=gpar(cex=0.5))
grid.draw(sg)



###################################################
### chunk number 19: 
###################################################
splot(1:10, 1:10, "Same as Before", name="splot")
grid.edit(gPath("splot", "points"), gp=gpar(col=1:10))



###################################################
### chunk number 20: 
###################################################
sg <- splot(1:10, 1:10, "Same as Before", name="splot", draw=FALSE)

sg <- editGrob(sg, gPath="points", gp=gpar(col=1:10))
grid.draw(sg)



###################################################
### chunk number 21: 
###################################################
editDetails.splot <- function(x, specs) {
  if (any(c("x", "y") %in% names(specs))) {
    if (is.null(specs$x))
      xx <- x$x
    else
      xx <- specs$x
    if (is.null(specs$y))
      yy <- x$y
    else
      yy <- specs$y
    x$childrenvp <- splot.data.vp(xx, yy)
    x <- addGrob(x, pointsGrob(xx, yy, name="points", vp="dataregion"))
  }
  x
}
splot(1:10, 1:10, "Same as Before", name="splot")
grid.edit("splot", x=1:100, y=(1:100)^2)



###################################################
### chunk number 22: 
###################################################
sg <- splot(1:10, 1:10, "Same as Before", name="splot", draw=FALSE)

sg <- editGrob(sg, x=1:100, y=(1:100)^2)
grid.draw(sg)



###################################################
### chunk number 23: 
###################################################
cellname <- function(i, j) {
  paste("cell", i, j, sep="")
}

splom.vpTree <- function(n) {
  vplist <- vector("list", n^2)
  for (i in 1:n)
    for (j in 1:n)
      vplist[[(i - 1)*n + j]] <-
        viewport(layout.pos.row=i, layout.pos.col=j,
          name=cellname(i, j))
  vpTree(viewport(layout=grid.layout(n, n), name="cellgrid"),
    do.call("vpList", vplist))
}

cellpath <- function(i, j) {
  vpPath("cellgrid", cellname(i, j))
}

splom <- function(df, name=NULL, draw=TRUE) {
  n <- dim(df)[2]
  glist <- vector("list", n*n)
  for (i in 1:n)
    for (j in 1:n)
      if (i == j)
        glist[[(i - 1)*n + j]] <- textGrob(paste("diag", i, sep=""),
          gp=gpar(col="grey"), vp=cellpath(i, j))
      else if (j > i)
        glist[[(i - 1)*n + j]] <- textGrob(cellname(i, j),
          name=cellname(i, j),
          gp=gpar(col="grey"), vp=cellpath(i, j))
      else
        glist[[(i - 1)*n + j]] <- splot(df[,j], df[,i], "",
          name=paste("plot", i, j, sep=""), vp=cellpath(i, j),
          gp=gpar(cex=0.5), draw=FALSE)
  smg <- gTree(name=name,
    childrenvp=splom.vpTree(n),
    children=do.call("gList", glist))
  if (draw)
    grid.draw(smg)
  smg
}

df <- data.frame(x=rnorm(10), y=rnorm(10), z=rnorm(10))
splom(df)



###################################################
### chunk number 24: 
###################################################
splom(df)
grid.edit("plot21::xlab", label="", redraw=FALSE)
grid.edit("plot32::ylab", label="", redraw=FALSE)
grid.edit("plot21::xaxis", label=FALSE, redraw=FALSE)
grid.edit("plot32::yaxis", label=FALSE)



###################################################
### chunk number 25: splomgrob eval=FALSE
###################################################
## smg <- splom(df, draw=FALSE)
## 


###################################################
### chunk number 26: 
###################################################
smg <- splom(df, draw=FALSE)

smg <- editGrob(smg, gPath="plot21::xaxis", label=FALSE)
smg <- editGrob(smg, gPath="plot21::xlab", label="")
smg <- editGrob(smg, gPath="plot32::yaxis", label=FALSE)
smg <- editGrob(smg, gPath="plot32::ylab", label="")
grid.draw(smg)



###################################################
### chunk number 27: 
###################################################
splom(df, name="splom")
grid.remove("cell12")
grid.add("splom", textGrob(date(), name="date", gp=gpar(fontface="italic"),
  vp="cellgrid::cell12"))



###################################################
### chunk number 28: 
###################################################
smg <- splom(df, draw=FALSE)

smg <- removeGrob(smg, "cell12")
smg <- addGrob(smg, textGrob(date(), name="date", gp=gpar(fontface="italic"),
  vp="cellgrid::cell12"))
grid.draw(smg)



###################################################
### chunk number 29: 
###################################################
splom(df, name="splom")
grid.remove("cell12")
grid.add("splom", textGrob(date(), name="date", gp=gpar(fontface="italic"),
  vp="cellgrid::cell12"))
smg <- grid.get("splom")
save(smg, file="splom.RData")
load("splom.RData")
plot <- getGrob(smg, "plot31")
date <- getGrob(smg, "date")
plot <- editGrob(plot, vp=NULL, gp=gpar(cex=1))
date <- editGrob(date, y=unit(1, "npc") - unit(1, "lines"), vp=NULL)
grid.newpage()
grid.draw(plot)
grid.draw(date)



###################################################
### chunk number 30: 
###################################################
smg <- splom(df, draw=FALSE)

smg <- removeGrob(smg, "cell12")
smg <- addGrob(smg, textGrob(date(), name="date", gp=gpar(fontface="italic"),
  vp="cellgrid::cell12"))
save(smg, file="splom.RData")
load("splom.RData")
plot <- getGrob(smg, "plot31")
date <- getGrob(smg, "date")
plot <- editGrob(plot, vp=NULL, gp=gpar(cex=1))
date <- editGrob(date, y=unit(1, "npc") - unit(1, "lines"), vp=NULL)
grid.draw(plot)
grid.draw(date)



###################################################
### chunk number 31:  eval=FALSE
###################################################
## grid.newpage()
## 


