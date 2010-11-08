# Changes for version 1.2
# bug fix:
# plotweb:
# bug: for adjy - only apparent if 4 boxes!
# was:
#  	    if (    yl[i]  > 0     ) adjusty = 0
#		    if (    yl[i]  < 0     ) adjusty = 1
# karline: bug fix - 06-03-2008 - ADDED THIS SENTENCE:
#		    if (abs(yl[i]) < 0.0001) adjusty = 0.5


# Addition
# suggestion from Yvonnic NOEL: to evaluate expressions and write the evaluated
# expressions next to arrow heads: code changed from
# if(cex.txt>0) text(ell[1,1],ell[1,2],txt,adj=adj,cex=cex.txt)
# to:
# if(cex.txt>0) text(ell[1,1],ell[1,2],parse(text=txt),adj=adj,cex=cex.txt)
# e.g.

 M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
 M     <- as.data.frame(M)
 M[[2,1]]<- "k[si]"
 M[[3,1]]<- "k[N]"
 M[[4,2]]<- "sqrt(frac(2,3))"

 names <-
 c(expression(lambda[12]),"?",expression(lambda[13]),expression(lambda[23]))

  pp<-plotmat(M,pos=c(1,2,1),name=names,lwd=1,box.lwd=2, curve=0,
              cex.txt=0.8,box.size=0.1,box.type="square",box.prop=0.5,
              main="plotmat")

# plotweb:
# nullflow can now be a vector: e.g. nullflow = c(0,100) will NOT draw the
# flows < 0 and > 100.