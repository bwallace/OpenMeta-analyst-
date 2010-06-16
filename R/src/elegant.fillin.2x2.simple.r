#fillin.2x2.simple <- function(
#c11=NA, 
#c12=NA, 
#c21=NA, 
#c22=NA, 
#
#r1sum =NA, 
#r2sum=NA, 
#c1sum=NA, 
#c2sum=NA,
#total=NA, 
#
#prop.c11.r1sum =NA,
#prop.c11.c1sum =NA,
#
#prop.c12.r1sum =NA,
#prop.c12.c2sum =NA,
#
#prop.c21.r2sum =NA,
#prop.c21.c1sum =NA,
#
#prop.c22.r2sum =NA,
#prop.c22.c2sum =NA,
#
#prop.r1sum.total =NA, 
#prop.r2sum.total =NA, 
#prop.c1sum.total =NA, 
#prop.c2sum.total =NA,
#
#touse=rep(TRUE,21)
#) {
#}

fillin.2x2.simple <- function(
c11=NA, 
c12=NA, 
c21=NA, 
c22=NA, 
r1sum =NA, 
r2sum=NA, 
c1sum=NA, 
c2sum=NA,
total=NA, 
touse=rep(TRUE,9)
) {
	y <- c(
	rep(c11,4), 
	rep(c12,4),
	rep(c21,4), 
	rep(c22,4),
	rep(r1sum,3), 
	rep(r2sum,3), 
	rep(c1sum,3), 
	rep(c2sum,3),
	rep(total,8)) 

    select <- c(
		rep(touse[1],4), 
		rep(touse[2],4), 
		rep(touse[3],4), 
		rep(touse[4],4), 
		rep(touse[5],3), 
		rep(touse[6],3), 
		rep(touse[7],3), 
		rep(touse[8],3), 
		rep(touse[9],8))

	X<- c(
	# c11, c12, c21, c22, r1sum, r2sum, c1sum, c2sum, total 
	    1,   0,   0,   0,     0,     0,     0,     0,     0,  #c11
	    0,  -1,   0,   0,     1,     0,     0,     0,     0,  #c11=r1sum-c12
	    0,   0,  -1,   0,     0,     0,     1,     0,     0,  #c11=c1sum-c21
	    0,  -1,  -1,  -1,     0,     0,     0,     0,     1,  #c11=total-c12-c21-c22

	    0,   1,   0,   0,     0,     0,     0,     0,     0,  #c12
	   -1,   0,   0,   0,     1,     0,     0,     0,     0,  #c12=r1sum-c11
	    0,   0,   0,  -1,     0,     0,     0,     1,     0,  #c12=c2sum-c22
	   -1,   0,  -1,  -1,     0,     0,     0,     0,     1,  #c12=total-c11-c21-c22

	    0,   0,   1,   0,     0,     0,     0,     0,     0,  #c21
	    0,   0,   0,  -1,     0,     1,     0,     0,     0,  #c21=r2sum-c22
	   -1,   0,   0,   0,     0,     0,     1,     0,     0,  #c21=c1sum-c11
	   -1,  -1,   0,  -1,     0,     0,     0,     0,     1,  #c21=total-c11-c12-c22

	    0,   0,   0,   1,     0,     0,     0,     0,     0,  #c22
	    0,   0,  -1,   0,     0,     1,     0,     0,     0,  #c22=r2sum-c21
	    0,  -1,   0,   0,     0,     0,     0,     1,     0,  #c22=c2sum-c12
	   -1,  -1,  -1,   0,     0,     0,     0,     0,     1,  #c22=total-c11-c12-c21
	
	    0,   0,   0,   0,     1,     0,     0,     0,     0,  #r1sum
	    1,   1,   0,   0,     0,     0,     0,     0,     0,  #r1sum=c11+c12
	    0,   0,   0,   0,     0,    -1,     0,     0,     1,  #r1sum=total-r2sum

	    0,   0,   0,   0,     0,     1,     0,     0,     0,  #r2sum
	    0,   0,   1,   1,     0,     0,     0,     0,     0,  #r2sum=c21+c22
	    0,   0,   0,   0,    -1,     0,     0,     0,     1,  #r2sum=total-r1sum
	    
	    0,   0,   0,   0,     0,     0,     1,     0,     0,  #c1sum
	    1,   0,   1,   0,     0,     0,     0,     0,     0,  #c1sum=c11+c21
	    0,   0,   0,   0,     0,     0,     0,    -1,     1,  #c1sum=total-c2sum

	    0,   0,   0,   0,     0,     0,     0,     1,     0,  #c2sum
	    0,   1,   0,   1,     0,     0,     0,     0,     0,  #c2sum=c12+c22
	    0,   0,   0,   0,     0,     0,    -1,     0,     1,  #c2sum=total-c1sum

	    0,   0,   0,   0,     0,     0,     0,     0,     1,  #total
	    1,   1,   1,   1,     0,     0,     0,     0,     0,  #total=c11+c12+c21+c22
	    0,   0,   0,   0,     1,     1,     0,     0,     0,  #total=r1sum+r2sum
	    0,   0,   0,   0,     0,     0,     1,     1,     0,  #total=c1sum+c2sum
	    0,   0,   1,   1,     1,     0,     0,     0,     0,  #total=r1sum+c21+c22
	    1,   1,   0,   0,     0,     1,     0,     0,     0,  #total=r2sum+c11+c12
	    0,   1,   0,   1,     0,     0,     1,     0,     0,  #total=c1sum+c12+c22
	    1,   0,   1,   0,     0,     0,     0,     1,     0   #total=c2sum+c11+c21
		)

	X<-matrix(X,ncol=9, byrow=TRUE)
	my.frame <- as.data.frame(X)
	colnames(my.frame) <- c("c11", "c12", "c21", "c22", "r1sum", "r2sum", "c1sum", "c2sum", "total" )
	
# add the responses y
	my.frame <- cbind(y, my.frame)
	
	res <- lm(y~ c11 + c12 + c21 + c22 + r1sum + r2sum + 
			  c1sum + c2sum + total + (-1) ,data=my.frame)
	
	return(res)
}
