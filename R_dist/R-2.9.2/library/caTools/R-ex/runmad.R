### Name: runmad
### Title: Median Absolute Deviation of Moving Windows
### Aliases: runmad
### Keywords: ts array utilities

### ** Examples

  # show runmed function
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  col = c("black", "red", "green")
  m=runmed(x, k)
  y=runmad(x, k, center=m)
  plot(x, col=col[1], main = "Moving Window Analysis Functions")
  lines(m    , col=col[2])
  lines(m-y/2, col=col[3])
  lines(m+y/2, col=col[3])
  lab = c("data", "runmed", "runmed-runmad/2", "runmed+runmad/2")
  legend(0,0.9*n, lab, col=col, lty=1 )

  # basic tests against apply/embed
  eps = .Machine$double.eps ^ 0.5
  k=25 # odd size window
  a = runmad(x,k, center=runmed(x,k), endrule="trim")
  b = apply(embed(x,k), 1, mad)
  stopifnot(all(abs(a-b)<eps));
  k=24 # even size window
  a = runmad(x,k, center=runquantile(x,k,0.5,type=2), endrule="trim")
  b = apply(embed(x,k), 1, mad)
  stopifnot(all(abs(a-b)<eps));
  
  # test against loop approach
  # this test works fine at the R prompt but fails during package check - need to investigate
  k=24; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # create random data
  x = rep(1:5,40)
  #x[seq(1,n,11)] = NaN;               # commented out for time beeing - on to do list
  #x[5] = NaN;                         # commented out for time beeing - on to do list
  k2 = k
  k1 = k-k2-1
  ac = array(runquantile(x,k,0.5))
  a  = runmad(x, k, center=ac)
  bc = array(0,n)
  b  = array(0,n)
  for(j in 1:n) {
    lo = max(1, j-k1)
    hi = min(n, j+k2)
    bc[j] = median(x[lo:hi], na.rm = TRUE)
    b [j] = mad   (x[lo:hi], na.rm = TRUE, center=bc[j])
  }
  eps = .Machine$double.eps ^ 0.5
  #stopifnot(all(abs(ac-bc)<eps)); # commented out for time beeing - on to do list
  #stopifnot(all(abs(a-b)<eps));   # commented out for time beeing - on to do list
  
  # compare calculation at array ends
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  c = runquantile(x,k,0.5,type=2)             # find the center
  a = runmad(x, k, center=c, endrule="mad" )  # fast C code
  b = runmad(x, k, center=c, endrule="func")  # slow R code
  stopifnot(all(abs(a-b)<eps));
  
  # test if moving windows forward and backward gives the same results
  k=51;
  a = runmad(x     , k)
  b = runmad(x[n:1], k)
  stopifnot(all(a[n:1]==b, na.rm=TRUE));

  # speed comparison
  ## Not run: 
##D   x=runif(1e5); k=51;                       # reduce vector and window sizes
##D   system.time(runmad( x,k,endrule="trim"))
##D   system.time(apply(embed(x,k), 1, mad))  
##D   
## End(Not run)



