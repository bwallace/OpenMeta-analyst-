###################################################
### chunk number 1: preliminaries
###################################################
library("diagram")
options(prompt = "> ")
options(width=90)


###################################################
### chunk number 2: plotmat1
###################################################
par(mar=c(1,1,1,1),mfrow=c(2,2))
#
#
names <- c("A","B","C","D")
M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
pp<-plotmat(M,pos=c(1,2,1),name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.size=0.1,box.type="square",box.prop=0.5)
#
#
M[2,1]<-M[3,1]<-M[4,2]<-M[4,3] <- "flow"
pp<-plotmat(M,pos=c(1,2,1),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="circle",box.prop=1.0)
#
#
diag(M) <- "self"
pp<-plotmat(M,pos=c(2,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            self.cex=0.5,self.shiftx=c(-0.1,0.1,-0.1,0.1),
            box.type="diamond",box.prop=0.5)

M <- matrix(nrow=4,ncol=4,data=0)
M[2,1]<-1  ;M[4,2]<-2;M[3,4]<-3;M[1,3]<-4

Col <- M
Col[] <- "black"
Col[4,2] <- "darkred"
pp<-plotmat(M,pos=c(1,2,1),curve=0.2,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            arr.type="triangle",box.size=0.1,box.type="hexa",box.prop=0.25,
            arr.col=Col,arr.len=1)
mtext(outer=TRUE,side=3,line=-1.5,cex=1.5,"plotmat")
#
par(mfrow=c(1,1))


###################################################
### chunk number 3: fig1
###################################################
par(mar=c(1,1,1,1),mfrow=c(2,2))
#
#
names <- c("A","B","C","D")
M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
pp<-plotmat(M,pos=c(1,2,1),name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.size=0.1,box.type="square",box.prop=0.5)
#
#
M[2,1]<-M[3,1]<-M[4,2]<-M[4,3] <- "flow"
pp<-plotmat(M,pos=c(1,2,1),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="circle",box.prop=1.0)
#
#
diag(M) <- "self"
pp<-plotmat(M,pos=c(2,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            self.cex=0.5,self.shiftx=c(-0.1,0.1,-0.1,0.1),
            box.type="diamond",box.prop=0.5)

M <- matrix(nrow=4,ncol=4,data=0)
M[2,1]<-1  ;M[4,2]<-2;M[3,4]<-3;M[1,3]<-4

Col <- M
Col[] <- "black"
Col[4,2] <- "darkred"
pp<-plotmat(M,pos=c(1,2,1),curve=0.2,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            arr.type="triangle",box.size=0.1,box.type="hexa",box.prop=0.25,
            arr.col=Col,arr.len=1)
mtext(outer=TRUE,side=3,line=-1.5,cex=1.5,"plotmat")
#
par(mfrow=c(1,1))


###################################################
### chunk number 4: plotmat2
###################################################
names <- c("PHYTO","NH3","ZOO","DETRITUS","BotDET","FISH")
M <- matrix(nrow=6,ncol=6,byrow=TRUE,data=c(
#   p n z  d  b  f
    0,1,0, 0, 0, 0, #p
    0,0,4, 10,11,0, #n
    2,0,0, 0, 0, 0, #z
    8,0,13,0, 0, 12,#d
    9,0,0, 7, 0, 0, #b
    0,0,5, 0, 0, 0  #f
    ))
#
pp<-plotmat(M,pos=c(1,2,1,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="square",box.prop=0.5,arr.type="triangle",
            arr.pos=0.4,shadow.size=0.01,prefix="f",
            main="NPZZDD model")
#
phyto   <-pp$comp[names=="PHYTO"]
zoo     <-pp$comp[names=="ZOO"]
nh3     <-pp$comp[names=="NH3"]
detritus<-pp$comp[names=="DETRITUS"]
fish    <-pp$comp[names=="FISH"]
#
# flow5->detritus
#
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1] <- m1[1]+ pp$radii[4,1]
mid <- straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.4,lwd=1)
text(mid[1],mid[2]+0.03,"f6",cex=0.8)
#
# flow2->detritus
#
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1] + pp$radii[3,1]*0.2
m1[2] <-m1[2] + pp$radii[3,2]
mid<-straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.3,lwd=1)
text(mid[1]-0.01,mid[2]+0.03,"f3",cex=0.8)


###################################################
### chunk number 5: fig2
###################################################
names <- c("PHYTO","NH3","ZOO","DETRITUS","BotDET","FISH")
M <- matrix(nrow=6,ncol=6,byrow=TRUE,data=c(
#   p n z  d  b  f
    0,1,0, 0, 0, 0, #p
    0,0,4, 10,11,0, #n
    2,0,0, 0, 0, 0, #z
    8,0,13,0, 0, 12,#d
    9,0,0, 7, 0, 0, #b
    0,0,5, 0, 0, 0  #f
    ))
#
pp<-plotmat(M,pos=c(1,2,1,2),curve=0,name=names,lwd=1,box.lwd=2,cex.txt=0.8,
            box.type="square",box.prop=0.5,arr.type="triangle",
            arr.pos=0.4,shadow.size=0.01,prefix="f",
            main="NPZZDD model")
#
phyto   <-pp$comp[names=="PHYTO"]
zoo     <-pp$comp[names=="ZOO"]
nh3     <-pp$comp[names=="NH3"]
detritus<-pp$comp[names=="DETRITUS"]
fish    <-pp$comp[names=="FISH"]
#
# flow5->detritus
#
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1] <- m1[1]+ pp$radii[4,1]
mid <- straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.4,lwd=1)
text(mid[1],mid[2]+0.03,"f6",cex=0.8)
#
# flow2->detritus
#
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1] + pp$radii[3,1]*0.2
m1[2] <-m1[2] + pp$radii[3,2]
mid<-straightarrow (to=m1,from=m2,arr.type="triangle",arr.pos=0.3,lwd=1)
text(mid[1]-0.01,mid[2]+0.03,"f3",cex=0.8)


###################################################
### chunk number 6: plotmat3
###################################################
# Create population matrix
#
Numgenerations   <- 6
DiffMat  <- matrix(data=0,nrow=Numgenerations,ncol=Numgenerations)
AA <- as.data.frame(DiffMat)
AA[[1,4]]<- "f[3]"
AA[[1,5]]<- "f[4]"
AA[[1,6]]<- "f[5]"
#
AA[[2,1]]<- "s[list(0,1)]"
AA[[3,2]]<- "s[list(1,2)]"
AA[[4,3]]<- "s[list(2,3)]"
AA[[5,4]]<- "s[list(3,4)]"
AA[[6,5]]<- "s[list(4,5)]"
#
name  <- c(expression(Age[0]),expression(Age[1]),expression(Age[2]),
           expression(Age[3]),expression(Age[4]),expression(Age[5]))
#
PP <- plotmat(A=AA,pos=6,curve=0.7,name=name,lwd=2,arr.len=0.6,
              arr.width=0.25,my=-0.2,
              box.size=0.05,arr.type="triangle",dtext= 0.95,
              main="Age-structured population model 1")


###################################################
### chunk number 7: fig3
###################################################
# Create population matrix
#
Numgenerations   <- 6
DiffMat  <- matrix(data=0,nrow=Numgenerations,ncol=Numgenerations)
AA <- as.data.frame(DiffMat)
AA[[1,4]]<- "f[3]"
AA[[1,5]]<- "f[4]"
AA[[1,6]]<- "f[5]"
#
AA[[2,1]]<- "s[list(0,1)]"
AA[[3,2]]<- "s[list(1,2)]"
AA[[4,3]]<- "s[list(2,3)]"
AA[[5,4]]<- "s[list(3,4)]"
AA[[6,5]]<- "s[list(4,5)]"
#
name  <- c(expression(Age[0]),expression(Age[1]),expression(Age[2]),
           expression(Age[3]),expression(Age[4]),expression(Age[5]))
#
PP <- plotmat(A=AA,pos=6,curve=0.7,name=name,lwd=2,arr.len=0.6,
              arr.width=0.25,my=-0.2,
              box.size=0.05,arr.type="triangle",dtext= 0.95,
              main="Age-structured population model 1")


###################################################
### chunk number 8: teasel
###################################################
Teasel


###################################################
### chunk number 9: plotmat4
###################################################
curves <- matrix(nrow=ncol(Teasel),ncol=ncol(Teasel),0)
curves[3,1]<- curves[1,6]<- -0.35
curves[4,6]<- curves[6,4]<- curves[5,6]<- curves[6,5]<-0.08
curves[3,6]<-  0.35

plotmat(Teasel,pos=c(3,2,1),curve=curves,name=colnames(Teasel),lwd=1,
        box.lwd=2,cex.txt=0.8,box.cex=0.8,box.size=0.08,arr.length=0.5,
        box.type="circle",box.prop=1,shadow.size = 0.01,self.cex=0.6,
        my=-0.075,mx=-0.01,relsize=0.9,self.shiftx=c(0,0,0.125,-0.12,0.125,0),
        self.shifty=0,main="Teasel population model")


###################################################
### chunk number 10: fig4
###################################################
curves <- matrix(nrow=ncol(Teasel),ncol=ncol(Teasel),0)
curves[3,1]<- curves[1,6]<- -0.35
curves[4,6]<- curves[6,4]<- curves[5,6]<- curves[6,5]<-0.08
curves[3,6]<-  0.35

plotmat(Teasel,pos=c(3,2,1),curve=curves,name=colnames(Teasel),lwd=1,
        box.lwd=2,cex.txt=0.8,box.cex=0.8,box.size=0.08,arr.length=0.5,
        box.type="circle",box.prop=1,shadow.size = 0.01,self.cex=0.6,
        my=-0.075,mx=-0.01,relsize=0.9,self.shiftx=c(0,0,0.125,-0.12,0.125,0),
        self.shifty=0,main="Teasel population model")


###################################################
### chunk number 11: plotweb1
###################################################
BB <- matrix(nrow=20,ncol=20,1:20)
diag(BB)<-0
Col <- BB
Col[] <- "black"
Col[BB<10]<- "red"
plotweb(BB,legend=TRUE,maxarrow=3,arr.col=Col)
par(mfrow=c(1,1))


###################################################
### chunk number 12: fig5
###################################################
BB <- matrix(nrow=20,ncol=20,1:20)
diag(BB)<-0
Col <- BB
Col[] <- "black"
Col[BB<10]<- "red"
plotweb(BB,legend=TRUE,maxarrow=3,arr.col=Col)
par(mfrow=c(1,1))


###################################################
### chunk number 13: 
###################################################
Rigaweb


###################################################
### chunk number 14: foodweb1
###################################################
plotweb(Rigaweb,main="Gulf of Riga food web",sub="mgC/m3/d",val=TRUE)


###################################################
### chunk number 15: fig6
###################################################
plotweb(Rigaweb,main="Gulf of Riga food web",sub="mgC/m3/d",val=TRUE)


###################################################
### chunk number 16: chart1
###################################################

par(mar=c(1,1,1,1))
openplotmat()
elpos  <-coordinates (c(1,1,2,4))
fromto <- matrix(ncol=2,byrow=TRUE,data=c(1,2,2,3,2,4,4,7,4,8))
nr     <-nrow(fromto)
arrpos <- matrix(ncol=2,nrow=nr)
for (i in 1:nr)
    arrpos[i,]<- straightarrow (to=elpos[fromto[i,2],],from=elpos[fromto[i,1],]
        ,lwd=2,arr.pos=0.6,arr.length=0.5)
textellipse(elpos[1,],0.1,      lab="start",           box.col="green",
            shadow.col="darkgreen",shadow.size=0.005,cex=1.5)
textrect   (elpos[2,],0.15,0.05,lab="found term?",     box.col="blue",
            shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textrect   (elpos[4,],0.15,0.05,lab="related?",        box.col="blue",
            shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,  lab=c("other","term"), box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,  lab=c("other","term"), box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[7,],0.1,0.1,  lab=c("make","a link"),box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[8,],0.1,0.1,  lab=c("new","article"),box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
#
dd <- c(0.0,0.025)
text(arrpos[2,1]+0.05,arrpos[2,2],"yes")
text(arrpos[3,1]-0.05,arrpos[3,2],"no")
text(arrpos[4,1]+0.05,arrpos[4,2]+0.05,"yes")
text(arrpos[5,1]-0.05,arrpos[5,2]+0.05,"no")
#


###################################################
### chunk number 17: fig7
###################################################

par(mar=c(1,1,1,1))
openplotmat()
elpos  <-coordinates (c(1,1,2,4))
fromto <- matrix(ncol=2,byrow=TRUE,data=c(1,2,2,3,2,4,4,7,4,8))
nr     <-nrow(fromto)
arrpos <- matrix(ncol=2,nrow=nr)
for (i in 1:nr)
    arrpos[i,]<- straightarrow (to=elpos[fromto[i,2],],from=elpos[fromto[i,1],]
        ,lwd=2,arr.pos=0.6,arr.length=0.5)
textellipse(elpos[1,],0.1,      lab="start",           box.col="green",
            shadow.col="darkgreen",shadow.size=0.005,cex=1.5)
textrect   (elpos[2,],0.15,0.05,lab="found term?",     box.col="blue",
            shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textrect   (elpos[4,],0.15,0.05,lab="related?",        box.col="blue",
            shadow.col="darkblue",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,  lab=c("other","term"), box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[3,],0.1,0.1,  lab=c("other","term"), box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[7,],0.1,0.1,  lab=c("make","a link"),box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
textellipse(elpos[8,],0.1,0.1,  lab=c("new","article"),box.col="orange",
            shadow.col="red",shadow.size=0.005,cex=1.5)
#
dd <- c(0.0,0.025)
text(arrpos[2,1]+0.05,arrpos[2,2],"yes")
text(arrpos[3,1]-0.05,arrpos[3,2],"no")
text(arrpos[4,1]+0.05,arrpos[4,2]+0.05,"yes")
text(arrpos[5,1]-0.05,arrpos[5,2]+0.05,"no")
#


###################################################
### chunk number 18: chart2
###################################################
openplotmat(main="textbox shapes")
rx <- 0.1
ry <- 0.05
pos <- coordinates(c(1,1,1,1,1,1,1),mx=-0.2)
textdiamond(mid=pos[1,],radx=rx,rady=ry,lab=LETTERS[1],cex=2,shadow.col="lightblue")
textellipse(mid=pos[2,],radx=rx,rady=ry,lab=LETTERS[2],cex=2,shadow.col="blue")
texthexa(mid=pos[3,],radx=rx,rady=ry,lab=LETTERS[3],cex=2,shadow.col="darkblue")
textmulti(mid=pos[4,],nr=7,radx=rx,rady=ry,lab=LETTERS[4],cex=2,shadow.col="red")
textrect(mid=pos[5,],radx=rx,rady=ry,lab=LETTERS[5],cex=2,shadow.col="darkred")
textround(mid=pos[6,],radx=rx,rady=ry,lab=LETTERS[6],cex=2,shadow.col="black")
textempty(mid=pos[7,],lab=LETTERS[7],cex=2,box.col="yellow")
pos[,1] <- pos[,1] + 0.5
text(pos[,1],pos[,2],c("textdiamond","textellipse","texthexa",
                       "textmulti","textrect","textround","textempty"))


###################################################
### chunk number 19: fig8
###################################################
openplotmat(main="textbox shapes")
rx <- 0.1
ry <- 0.05
pos <- coordinates(c(1,1,1,1,1,1,1),mx=-0.2)
textdiamond(mid=pos[1,],radx=rx,rady=ry,lab=LETTERS[1],cex=2,shadow.col="lightblue")
textellipse(mid=pos[2,],radx=rx,rady=ry,lab=LETTERS[2],cex=2,shadow.col="blue")
texthexa(mid=pos[3,],radx=rx,rady=ry,lab=LETTERS[3],cex=2,shadow.col="darkblue")
textmulti(mid=pos[4,],nr=7,radx=rx,rady=ry,lab=LETTERS[4],cex=2,shadow.col="red")
textrect(mid=pos[5,],radx=rx,rady=ry,lab=LETTERS[5],cex=2,shadow.col="darkred")
textround(mid=pos[6,],radx=rx,rady=ry,lab=LETTERS[6],cex=2,shadow.col="black")
textempty(mid=pos[7,],lab=LETTERS[7],cex=2,box.col="yellow")
pos[,1] <- pos[,1] + 0.5
text(pos[,1],pos[,2],c("textdiamond","textellipse","texthexa",
                       "textmulti","textrect","textround","textempty"))


###################################################
### chunk number 20: chart3
###################################################
par(mar=c(1,1,1,1))
openplotmat(main="Arrowtypes")
elpos<-coordinates (c(1,2,1),mx=0.1,my=-0.1)
curvedarrow(from=elpos[1,],to=elpos[2,],curve=-0.5,lty=2,lcol=2)
straightarrow(from=elpos[1,],to=elpos[2,],lty=3,lcol=3)
segmentarrow(from=elpos[1,],to=elpos[2,],lty=1,lcol=1)
treearrow(from=elpos[2:3,],to=elpos[4,],lty=4,lcol=4)
bentarrow(from=elpos[3,],to=elpos[3,]-c(0.1,0.1),arr.pos=1,lty=5,lcol=5)
bentarrow(from=elpos[1,],to=elpos[3,],lty=5,lcol=5)
selfarrow(pos=elpos[3,],path="R",lty=6,curve=0.075,lcol=6)
splitarrow(from=elpos[1,],to=elpos[2:3,],lty=1,lwd=1,dd=0.7,arr.side=1:2,lcol=7)

for ( i in 1:4) textrect (elpos[i,],0.05,0.05,lab=i,cex=1.5)

legend("topright",lty=1:7,legend=c("segmentarrow","curvedarrow","straightarrow",
"treearrow","bentarrow","selfarrow","splitarrow"),lwd=c(rep(2,6),1),col=1:7)


###################################################
### chunk number 21: fig9
###################################################
par(mar=c(1,1,1,1))
openplotmat(main="Arrowtypes")
elpos<-coordinates (c(1,2,1),mx=0.1,my=-0.1)
curvedarrow(from=elpos[1,],to=elpos[2,],curve=-0.5,lty=2,lcol=2)
straightarrow(from=elpos[1,],to=elpos[2,],lty=3,lcol=3)
segmentarrow(from=elpos[1,],to=elpos[2,],lty=1,lcol=1)
treearrow(from=elpos[2:3,],to=elpos[4,],lty=4,lcol=4)
bentarrow(from=elpos[3,],to=elpos[3,]-c(0.1,0.1),arr.pos=1,lty=5,lcol=5)
bentarrow(from=elpos[1,],to=elpos[3,],lty=5,lcol=5)
selfarrow(pos=elpos[3,],path="R",lty=6,curve=0.075,lcol=6)
splitarrow(from=elpos[1,],to=elpos[2:3,],lty=1,lwd=1,dd=0.7,arr.side=1:2,lcol=7)

for ( i in 1:4) textrect (elpos[i,],0.05,0.05,lab=i,cex=1.5)

legend("topright",lty=1:7,legend=c("segmentarrow","curvedarrow","straightarrow",
"treearrow","bentarrow","selfarrow","splitarrow"),lwd=c(rep(2,6),1),col=1:7)


