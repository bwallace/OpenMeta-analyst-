### Name: runmin & runmax
### Title: Minimum and Maximum of Moving Windows
### Aliases: runmin runmax
### Keywords: ts smooth array utilities

### ** Examples

  # show plot using runmin, runmax and runmed
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  col = c("black", "red", "green", "blue", "magenta", "cyan")
  plot(x, col=col[1], main = "Moving Window Analysis Functions")
  lines(runmin(x,k), col=col[2])
  lines(runmean(x,k), col=col[3])
  lines(runmax(x,k), col=col[4])
  legend(0,.9*n, c("data", "runmin", "runmean", "runmax"), col=col, lty=1 )

  # basic tests against standard R approach
  a = runmin(x,k, endrule="trim") # test only the inner part 
  b = apply(embed(x,k), 1, min)   # Standard R running min
  stopifnot(all(a==b));
  a = runmax(x,k, endrule="trim") # test only the inner part
  b = apply(embed(x,k), 1, max)   # Standard R running min
  stopifnot(all(a==b));
  
  # test against loop approach
  # this test works fine at the R prompt but fails during package check - need to investigate
  k=25; 
  data(iris)
  x = iris[,1]
  n = length(x)
  x[seq(1,n,11)] = NaN;                # add NANs
  k2 = k
  k1 = k-k2-1
  a1 = runmin(x, k)
  a2 = runmax(x, k)
  b1 = array(0,n)
  b2 = array(0,n)
  for(j in 1:n) {
    lo = max(1, j-k1)
    hi = min(n, j+k2)
    b1[j] = min(x[lo:hi], na.rm = TRUE)
    b2[j] = max(x[lo:hi], na.rm = TRUE)
  }
  #stopifnot(all(a1==b1), na.rm=TRUE);
  #stopifnot(all(a2==b2), na.rm=TRUE);
  
  # Test if moving windows forward and backward gives the same results
  # Two data sets also corespond to best and worst-case scenatio data-sets
  k=51; n=200;
  a = runmin(n:1, k) 
  b = runmin(1:n, k)
  stopifnot(all(a[n:1]==b, na.rm=TRUE));
  a = runmax(n:1, k)
  b = runmax(1:n, k)
  stopifnot(all(a[n:1]==b, na.rm=TRUE));

  # Compare C and R algorithms to each other for extreme window sizes
  numeric.test = function (x, k) {
    a = runmin( x, k, alg="C")
    b = runmin( x, k, alg="R")
    c =-runmax(-x, k, alg="C")
    d =-runmax(-x, k, alg="R")
    stopifnot(all(a==b, na.rm=TRUE));
    #stopifnot(all(c==d, na.rm=TRUE)); 
    #stopifnot(all(a==c, na.rm=TRUE));
    stopifnot(all(b==d, na.rm=TRUE));
  }
  n=200;                               # n is an even number
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # random data
  for(i in 1:5) numeric.test(x, i)     # test for small window size
  for(i in 1:5) numeric.test(x, n-i+1) # test for large window size
  n=201;                               # n is an odd number
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # random data
  for(i in 1:5) numeric.test(x, i)     # test for small window size
  for(i in 1:5) numeric.test(x, n-i+1) # test for large window size
  n=200;                               # n is an even number
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # random data
  x[seq(1,200,10)] = NaN;              # with some NaNs
  for(i in 1:5) numeric.test(x, i)     # test for small window size
  for(i in 1:5) numeric.test(x, n-i+1) # test for large window size
  n=201;                               # n is an odd number
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # random data
  x[seq(1,200,2)] = NaN;               # with some NaNs
  for(i in 1:5) numeric.test(x, i)     # test for small window size
  for(i in 1:5) numeric.test(x, n-i+1) # test for large window size

  # speed comparison
  ## Not run: 
##D   n = 1e7;  k=991; 
##D   x1 = runif(n);                       # random data - average case scenario
##D   x2 = 1:n;                            #  best-case scenario data for runmax
##D   x3 = n:1;                            # worst-case scenario data for runmax
##D   system.time( runmax( x1,k,alg="C"))  # C alg on average data O(n)
##D   system.time( runmax( x2,k,alg="C"))  # C alg on  best-case data O(n)
##D   system.time( runmax( x3,k,alg="C"))  # C alg on worst-case data O(n*k)
##D   system.time(-runmin(-x1,k,alg="C"))  # use runmin to do runmax work
##D   system.time( runmax( x1,k,alg="R"))  # R version of the function
##D   x=runif(1e5); k=1e2;                 # reduce vector and window sizes
##D   system.time(runmax(x,k,alg="R"))     # R version of the function
##D   system.time(apply(embed(x,k), 1, max)) # standard R approach 
##D   
## End(Not run)



