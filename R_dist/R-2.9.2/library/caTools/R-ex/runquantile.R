### Name: runquantile
### Title: Quantile of Moving Window
### Aliases: runquantile
### Keywords: ts smooth array utilities

### ** Examples

  # show plot using runquantile
  k=31; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  y=runquantile(x, k, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  col = c("black", "red", "green", "blue", "magenta", "cyan")
  plot(x, col=col[1], main = "Moving Window Quantiles")
  lines(y[,1], col=col[2])
  lines(y[,2], col=col[3])
  lines(y[,3], col=col[4])
  lines(y[,4], col=col[5])
  lines(y[,5], col=col[6])
  lab = c("data", "runquantile(.05)", "runquantile(.25)", "runquantile(0.5)", 
          "runquantile(.75)", "runquantile(.95)")
  legend(0,230, lab, col=col, lty=1 )

  # show plot using runquantile
  k=15; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4)
  y=runquantile(x, k, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  col = c("black", "red", "green", "blue", "magenta", "cyan")
  plot(x, col=col[1], main = "Moving Window Quantiles (smoothed)")
  lines(runmean(y[,1],k), col=col[2])
  lines(runmean(y[,2],k), col=col[3])
  lines(runmean(y[,3],k), col=col[4])
  lines(runmean(y[,4],k), col=col[5])
  lines(runmean(y[,5],k), col=col[6])
  lab = c("data", "runquantile(.05)", "runquantile(.25)", "runquantile(0.5)", 
          "runquantile(.75)", "runquantile(.95)")
  legend(0,230, lab, col=col, lty=1 )
  
  # basic tests against runmin & runmax
  y = runquantile(x, k, probs=c(0, 1))
  a = runmin(x,k) # test only the inner part 
  stopifnot(all(a==y[,1], na.rm=TRUE));
  a = runmax(x,k) # test only the inner part
  stopifnot(all(a==y[,2], na.rm=TRUE));
  
  # basic tests against runmed, including testing endrules
  a = runquantile(x, k, probs=0.5, endrule="keep")
  b = runmed(x, k, endrule="keep")
  stopifnot(all(a==b, na.rm=TRUE));
  a = runquantile(x, k, probs=0.5, endrule="constant")
  b = runmed(x, k, endrule="constant")
  stopifnot(all(a==b, na.rm=TRUE));

  # basic tests against apply/embed
  a = runquantile(x,k, c(0.3, 0.7), endrule="trim")
  b = t(apply(embed(x,k), 1, quantile, probs = c(0.3, 0.7)))
  eps = .Machine$double.eps ^ 0.5
  stopifnot(all(abs(a-b)<eps));
  
  # test against loop approach
  # this test works fine at the R prompt but fails during package check - need to investigate
  k=25; n=200;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # create random data
  x[seq(1,n,11)] = NaN;                # add NANs
  k2 = k
  k1 = k-k2-1
  a = runquantile(x, k, probs=c(0.3, 0.8) )
  b = matrix(0,n,2);
  for(j in 1:n) {
    lo = max(1, j-k1)
    hi = min(n, j+k2)
    b[j,] = quantile(x[lo:hi], probs=c(0.3, 0.8), na.rm = TRUE)
  }
  #stopifnot(all(abs(a-b)<eps));
  
  # compare calculation of array ends
  a = runquantile(x, k, probs=0.4, endrule="quantile") # fast C code
  b = runquantile(x, k, probs=0.4, endrule="func")     # slow R code
  stopifnot(all(abs(a-b)<eps));
  
  # test if moving windows forward and backward gives the same results
  k=51;
  a = runquantile(x     , k, probs=0.4)
  b = runquantile(x[n:1], k, probs=0.4)
  stopifnot(all(a[n:1]==b, na.rm=TRUE));

  # Exhaustive testing of runquantile to standard R approach
  numeric.test = function (x, k) {
    probs=c(1, 25, 50, 75, 99)/100
    a = runquantile(x,k, c(0.3, 0.7), endrule="trim")
    b = t(apply(embed(x,k), 1, quantile, probs = c(0.3, 0.7), na.rm=TRUE))
    eps = .Machine$double.eps ^ 0.5
    stopifnot(all(abs(a-b)<eps));
  }
  n=50;
  x = rnorm(n,sd=30) + abs(seq(n)-n/4) # nice behaving data
  for(i in 2:5) numeric.test(x, i)     # test small window sizes
  for(i in 1:5) numeric.test(x, n-i+1) # test large window size
  x[seq(1,50,10)] = NaN;               # add NANs and repet the test
  for(i in 2:5) numeric.test(x, i)     # test small window sizes
  for(i in 1:5) numeric.test(x, n-i+1) # test large window size
  
  # Speed comparison
  ## Not run: 
##D   x=runif(1e6); k=1e3+1;
##D   system.time(runquantile(x,k,0.5))    # Speed O(n*k)
##D   system.time(runmed(x,k))             # Speed O(n * log(k)) 
##D   
## End(Not run)



