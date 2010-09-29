###################################################
### chunk number 1: 
###################################################
library(grDevices)
library(stats) # for runif
library(grid)
ps.options(pointsize=12)
options(width=60)



###################################################
### chunk number 2: 
###################################################
gf <- frameGrob()



###################################################
### chunk number 3: 
###################################################
gf <- packGrob(gf, textGrob("Hello frame!"))



###################################################
### chunk number 4: frame1
###################################################
st <- grid.text("some text")
grid.rect(width=unit(1, "grobwidth", st),
          height=unit(1, "grobheight", st))



###################################################
### chunk number 5: frame1
###################################################
grid.text("some text", name="st")
grid.rect(width=unit(1, "grobwidth", "st"),
          height=unit(1, "grobheight", "st"))



###################################################
### chunk number 6: 
###################################################
grid.edit("st", gp=gpar(fontsize=20))



###################################################
### chunk number 7: frame2
###################################################
my.text <- textGrob("some text")
my.text <- editGrob(my.text, gp=gpar(fontsize=20))
my.rect <- rectGrob(width=unit(1, "grobwidth", my.text),
          height=unit(1, "grobheight", my.text))
grid.draw(my.text)
grid.draw(my.rect)



###################################################
### chunk number 8: 
###################################################
grid.edit("st", label="some different text")



###################################################
### chunk number 9: frame3
###################################################
my.text <- textGrob("some text")
my.text <- editGrob(my.text, gp=gpar(fontsize=20))
my.text <- editGrob(my.text, label="some different text")
my.rect <- rectGrob(width=unit(1, "grobwidth", my.text),
          height=unit(1, "grobheight", my.text))
grid.draw(my.text)
grid.draw(my.rect)



###################################################
### chunk number 10: 
###################################################
legendGrob <- function(pch, labels, frame=TRUE,
                        hgap=unit(1, "lines"), vgap=unit(1, "lines"),
                        default.units="lines",
                        vp=NULL) {
  nkeys <- length(labels)
  gf <- frameGrob(vp=vp)
  for (i in 1:nkeys) {
    if (i == 1) {
        symbol.border <- unit.c(vgap, hgap, vgap, hgap)
        text.border <- unit.c(vgap, unit(0, "npc"), vgap,
            hgap)
    }
    else {
        symbol.border <- unit.c(vgap, hgap, unit(0, "npc"),
            hgap)
        text.border <- unit.c(vgap, unit(0, "npc"), unit(0,
            "npc"), hgap)
    }
    gf <- packGrob(gf, pointsGrob(0.5, 0.5, pch=pch[i]),
        col=1, row=i, border=symbol.border, width=unit(1,
            "lines"), height=unit(1, "lines"), force.width=TRUE)
    gf <- packGrob(gf, textGrob(labels[i], x=0, y=0.5, just=c("left",
        "centre")), col=2, row=i, border=text.border)
  }
  gf
}

grid.legend <- function(pch, labels, frame=TRUE,
                        hgap=unit(1, "lines"), vgap=unit(1, "lines"),
                        default.units="lines", draw=TRUE,
                        vp=NULL) {
  gf <- legendGrob(pch, labels, frame, hgap, vgap, default.units, vp)
  if (draw)
    grid.draw(gf)
  gf
}



###################################################
### chunk number 11: legend
###################################################
grid.legend(1:3, c("one line", "two\nlines", "three\nlines\nof text"))



###################################################
### chunk number 12: plot
###################################################
  top.vp <- viewport(width=0.8, height=0.8)
  pushViewport(top.vp)
  x <- runif(10)
  y1 <- runif(10)
  y2 <- runif(10)
  pch <- 1:3
  labels <- c("Girls", "Boys", "Other")
  gf <- frameGrob()
  plt <- gTree(children=gList(rectGrob(),
                          pointsGrob(x, y1, pch=1),
                          pointsGrob(x, y2, pch=2),
                          xaxisGrob(),
                          yaxisGrob()))
  gf <- packGrob(gf, plt)
  gf <- packGrob(gf, legendGrob(pch, labels),
            height=unit(1, "null"), side="right")
  grid.rect(gp=gpar(col="grey"))
  grid.draw(gf)
  popViewport()
  grid.rect(gp=gpar(lty="dashed"), width=.99, height=.99)



###################################################
### chunk number 13:  eval=FALSE
###################################################
## grid.newpage()
## 


