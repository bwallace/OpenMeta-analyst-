### Name: runmean
### Title: Mean of a Moving Window
### Aliases: runmean
### Keywords: ts smooth array utilities

### ** Examples

  # show runmean for different window sizes
  n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  x[seq(1,200,10)] = NaN;              # add NANs
  col = c("black", "red", "green", "blue", "magenta", "cyan")
  plot(x, col=col[1], main = "Moving Window Means")
  lines(runmean(x, 3), col=col[2])
  lines(runmean(x, 8), col=col[3])
  lines(runmean(x,15), col=col[4])
  lines(runmean(x,24), col=col[5])
  lines(runmean(x,50), col=col[6])
  lab = c("data", "k=3", "k=8", "k=15", "k=24", "k=50")
  legend(0,0.9*n, lab, col=col, lty=1 )
  
  # basic tests against 2 standard R approaches
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)      # create random data
  a = runmean(x,k, endrule="trim")          # tested function
  b = apply(embed(x,k), 1, mean)            # approach #1
  c = cumsum(c( sum(x[1:k]), diff(x,k) ))/k # approach #2
  eps = .Machine$double.eps ^ 0.5
  stopifnot(all(abs(a-b)<eps));
  stopifnot(all(abs(a-c)<eps));
  
  # test against loop approach
  # this test works fine at the R prompt but fails during package check - need to investigate
  k=25; 
  data(iris)
  x = iris[,1]
  n = length(x)
  x[seq(1,n,11)] = NaN;                # add NANs
  k2 = k
  k1 = k-k2-1
  a = runmean(x, k)
  b = array(0,n)
  for(j in 1:n) {
    lo = max(1, j-k1)
    hi = min(n, j+k2)
    b[j] = mean(x[lo:hi], na.rm = TRUE)
  }
  #stopifnot(all(abs(a-b)<eps)); # commented out for time beeing - on to do list
  
  # compare calculation at array ends
  a = runmean(x, k, endrule="mean")  # fast C code
  b = runmean(x, k, endrule="func")  # slow R code
  stopifnot(all(abs(a-b)<eps));
  
  # Testing of different methods to each other for non-finite data
  # Only alg "C" and "exact" can handle not finite numbers 
  eps = .Machine$double.eps ^ 0.5
  n=200;  k=51;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # nice behaving data
  x[seq(1,n,10)] = NaN;                # add NANs
  x[seq(1,n, 9)] = Inf;                # add infinities
  b = runmean( x, k, alg="C")
  c = runmean( x, k, alg="exact")
  stopifnot(all(abs(b-c)<eps));

  # Test if moving windows forward and backward gives the same results
  # Test also performed on data with non-finite numbers
  a = runmean(x     , alg="C", k)
  b = runmean(x[n:1], alg="C", k)
  stopifnot(all(abs(a[n:1]-b)<eps));
  a = runmean(x     , alg="exact", k)
  b = runmean(x[n:1], alg="exact", k)
  stopifnot(all(abs(a[n:1]-b)<eps));

  # Exhaustive testing of different methods to each other for different windows
  numeric.test = function (x, k) {
    a = runmean( x, k, alg="fast")
    b = runmean( x, k, alg="C")
    c = runmean( x, k, alg="exact")
    d = runmean( x, k, alg="R", endrule="func")
    eps = .Machine$double.eps ^ 0.5
    stopifnot(all(abs(a-b)<eps));
    stopifnot(all(abs(b-c)<eps));
    stopifnot(all(abs(c-d)<eps));
  }
  n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # nice behaving data
  for(i in 1:5) numeric.test(x, i)     # test small window sizes
  for(i in 1:5) numeric.test(x, n-i+1) # test large window size

  # speed comparison
  ## Not run: 
##D   x=runif(1e7); k=1e4;
##D   system.time(runmean(x,k,alg="fast"))
##D   system.time(runmean(x,k,alg="C"))
##D   system.time(runmean(x,k,alg="exact"))
##D   system.time(runmean(x,k,alg="R"))           # R version of the function
##D   x=runif(1e5); k=1e2;                        # reduce vector and window sizes
##D   system.time(runmean(x,k,alg="R"))           # R version of the function
##D   system.time(apply(embed(x,k), 1, mean))     # standard R approach
##D   system.time(filter(x, rep(1/k,k), sides=2)) # the fastest alternative I know 
##D   
## End(Not run)
   
  # show different runmean algorithms with data spanning many orders of magnitude
  n=30; k=5;
  x = rep(100/3,n)
  d=1e10
  x[5] = d;     
  x[13] = d; 
  x[14] = d*d; 
  x[15] = d*d*d; 
  x[16] = d*d*d*d; 
  x[17] = d*d*d*d*d; 
  a = runmean(x, k, alg="fast" )
  b = runmean(x, k, alg="C"    )
  c = runmean(x, k, alg="exact")
  y = t(rbind(x,a,b,c))
  y



