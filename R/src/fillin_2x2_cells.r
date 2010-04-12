# see end for an example code


find.my.cells <- function(cell.counts, logor, var, 
					row1sum, row2sum, col1sum, col2sum) {
	
	# first equation: c11 + c12 = N1*
	eq1 <- row.total.eq(cell.counts, row=1, total=row1sum)
	
	# second equation: c21 + c22 = N2*
	eq2 <- row.total.eq(cell.counts, row=2, total=row2sum)

	# third equation: c11 + c21 = N*1
	eq3 <- col.total.eq(cell.counts, col=1, total=col1sum)
	
	# fourth equation: c12 + c22 = N*2
	eq4 <- col.total.eq(cell.counts, col=2, total=col2sum)
	
	# fifth equation: log(c11*c22)-log(c12*c21) = logor
	eq5 <- logor.eq(cell.counts, logor= logor)
	
	# sixth equation: 1/c11 + 1/c12 + 1/c21 + 1/c22 = var 
	eq6 <- var.eq(cell.counts, var = var)
	
	res <- eq1 + eq2 + eq3 + eq4 + eq5 + eq6
	return(1000*res)
}



row.total.eq <- function(cell.counts, row, total=NA) {
	res <- cell.counts[(row-1)*2+1] + cell.counts[(row-1)*2+2]
	if (is.na(total)) {
		return(res)
	}
	else {
		return((res-total)^2)
	}
	
} 


col.total.eq <- function(cell.counts, col, total=NA) {
	res <- cell.counts[col] + cell.counts[2+col]
	if (is.na(total)) {
		return(res)
	}
	else {
		return((res-total)^2)
	}
	
} 


logor.eq <- function(cell.counts, logor=NA, cc=0.5) {
	if (cell.counts[1]*cell.counts[2]*cell.counts[3]*cell.counts[4] !=0) {
		res <- log(cell.counts[1]*cell.counts[4]) -
		       log(cell.counts[2]*cell.counts[3])
	}
	else {
		res <- log(cell.counts[1]+cc) +  
		       log(cell.counts[4]+cc) -
		       log(cell.counts[2]+cc) -
		       log(cell.counts[3]+cc)
	}
	
	if (is.na(logor)) {
		return(res)
	}
	else {
		return((res - logor)^2)
	}
}
	
var.eq <- function(cell.counts, var=NA, cc=0.5) {
	if (cell.counts[1]*cell.counts[2]*cell.counts[3]*cell.counts[4] >0) {
		res <- 1/cell.counts[1] +
		       1/cell.counts[2] +
		       1/cell.counts[3] +
		       1/cell.counts[4]
	}
	else {
		res <- 1/(cell.counts[1]+cc) +
			   1/(cell.counts[2]+cc) +
			   1/(cell.counts[3]+cc) + 
			   1/(cell.counts[4]+cc)
	}
	if (is.na(var)) {
		return(res)		
	}
	else {
		return((res - var)^2)
	}
}


#var.sq.gradients <- function(cell.counts, var = NA) {
#	# returns partial first derivatives of squared variances 
#
#	d1 <- -2 * var.eq(cell.counts, var=var) / cell.counts[1]^2
#	d2 <- -2 * var.eq(cell.counts, var=var) / cell.counts[2]^2
#	d3 <- -2 * var.eq(cell.counts, var=var) / cell.counts[3]^2
#	d4 <- -2 * var.eq(cell.counts, var=var) / cell.counts[4]^2
#	res <- c(d1,d2,d3,d4)
#	
#	return(res)
#}

#logor.sq.gradients <- function(cell.counts, logor = NA) {
#	# returns partial first derivatives of squared logor
#
#	d1 <-  2 * logor.eq(cell.counts, logor=logor) / cell.counts[1]
#	d2 <- -2 * logor.eq(cell.counts, logor=logor) / cell.counts[2]
#	d3 <- -2 * logor.eq(cell.counts, logor=logor) / cell.counts[3]
#	d4 <-  2 * logor.eq(cell.counts, logor=logor) / cell.counts[4]
#	
#	res <- c(d1,d2,d3,d4)
#	
#	return(res)
#}



#gradients<- function(cell.counts, logor, var, 
#					row1sum, row2sum) {
#	# returns the gradients for the cell counts 
#	# note that I have to create them from the 
#	# four components of the 
#	# objective function. 	
#	
#	row1.sum.part <- 2*c(rep((sum(cell.counts[1:2]) - row1sum),2), 0, 0)
#	row2.sum.part <- 2*c(0 , 0, rep((sum(cell.counts[3:4]) - row2sum),2)) 
#	variance.part <- var.sq.gradients(cell.counts, var = var)
#	logor.part <- logor.sq.gradients(cell.counts, logor = logor)
#	
#	res <- row1.sum.part + row2.sum.part 
#		   variance.part + logor.part 
#	
#	return(res)
#}


############################################

# make a fictitious 2x2
a <- c(1001, 20, 300, 40)
# calculate the logor and the variance 
my.logor <- logor.eq(a, logor=NA)
my.var <- var.eq(a, var=NA)
my.row1sum <- sum(a[c(1,2)])
my.row2sum <- sum(a[c(3,4)])
my.col1sum <- sum(a[c(1,3)])
my.col2sum <- sum(a[c(2,4)])
my.total <- sum(a)

result<-optim(rep(my.total/4,4), find.my.cells, gr=NULL, logor=my.logor,var=my.var,
	row1sum=my.row1sum, row2sum =my.row2sum, col1sum=my.col1sum, col2sum=my.col2sum,
	method = "Nelder-Mead",
	control=c(abstol=1e-8, maxit=10000, reltol=0))

result

round(result$par)


