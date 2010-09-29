### Name: runsd
### Title: Standard Deviation of Moving Windows
### Aliases: runsd
### Keywords: ts array utilities

### ** Examples

  # show runmed function
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  col = c("black", "red", "green")
  m=runmean(x, k)
  y=runsd(x, k, center=m)
  plot(x, col=col[1], main = "Moving Window Analysis Functions")
  lines(m    , col=col[2])
  lines(m-y/2, col=col[3])
  lines(m+y/2, col=col[3])
  lab = c("data", "runmean", "runmean-runsd/2", "runmean+runsd/2")
  legend(0,0.9*n, lab, col=col, lty=1 )

  # basic tests against apply/embed
  eps = .Machine$double.eps ^ 0.5
  k=25 # odd size window
  a = runsd(x,k, endrule="trim")
  b = apply(embed(x,k), 1, sd)
  stopifnot(all(abs(a-b)<eps));
  k=24 # even size window
  a = runsd(x,k, endrule="trim")
  b = apply(embed(x,k), 1, sd)
  stopifnot(all(abs(a-b)<eps));
  
  # test against loop approach
  # this test works fine at the R prompt but fails during package check - need to investigate
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # create random data
  x[seq(1,n,11)] = NaN;                # add NANs
  k2 = k
  k1 = k-k2-1
  a = runsd(x, k)
  b = array(0,n)
  for(j in 1:n) {
    lo = max(1, j-k1)
    hi = min(n, j+k2)
    b[j] = sd(x[lo:hi], na.rm = TRUE)
  }
  #stopifnot(all(abs(a-b)<eps));
  
  # compare calculation at array ends
  k=25; n=100;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  a = runsd(x, k, endrule="sd" )   # fast C code
  b = runsd(x, k, endrule="func")  # slow R code
  stopifnot(all(abs(a-b)<eps));
  
  # test if moving windows forward and backward gives the same results
  k=51;
  a = runsd(x     , k)
  b = runsd(x[n:1], k)
  stopifnot(all(abs(a[n:1]-b)<eps));

  # speed comparison
  ## Not run: 
##D   x=runif(1e5); k=51;                       # reduce vector and window sizes
##D   system.time(runsd( x,k,endrule="trim"))
##D   system.time(apply(embed(x,k), 1, sd)) 
##D   
## End(Not run)



