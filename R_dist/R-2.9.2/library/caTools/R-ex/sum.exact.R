### Name: sum.exact, cumsum.exact & runsum.exact
### Title: Basic Sum Operations without Round-off Errors
### Aliases: sum.exact cumsum.exact runsum.exact
### Keywords: ts smooth array utilities

### ** Examples

  x = c(1, 1e20, 1e40, -1e40, -1e20, -1)
  a = sum(x);          print(a)
  b = sum.exact(x);    print(b)
  stopifnot(b==0)
  a = cumsum(x);       print(a)
  b = cumsum.exact(x); print(b)
  stopifnot(b[6]==0)



