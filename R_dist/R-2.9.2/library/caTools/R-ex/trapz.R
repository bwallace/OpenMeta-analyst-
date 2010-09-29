### Name: trapz
### Title: Trapezoid Rule Numerical Integration
### Aliases: trapz
### Keywords: math

### ** Examples

  # integral of sine function in [0, pi] range suppose to be exactly 2.
  # lets calculate it using 10 samples:
  x = (1:10)*pi/10
  trapz(x, sin(x))
  # now lets calculate it using 1000 samples:
  x = (1:1000)*pi/1000
  trapz(x, sin(x))



