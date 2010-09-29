### Name: base64encode & base64decode
### Title: Convert R vectors to/from the Base64 format
### Aliases: base64encode base64decode
### Keywords: file

### ** Examples

   x = (10*runif(10)>5) # logical
   for (i in c(NA, 1, 2, 4)) {
     y = base64encode(x, size=i)
     z = base64decode(y, typeof(x), size=i)
     stopifnot(x==z)
   }
   print("Checked base64 for encode/decode logical type")

   x = as.integer(1:10) # integer
   for (i in c(NA, 1, 2, 4)) {
     y = base64encode(x, size=i)
     z = base64decode(y, typeof(x), size=i)
     stopifnot(x==z)
   }
   print("Checked base64 encode/decode for integer type")
   
   x = (1:10)*pi        # double
   for (i in c(NA, 4, 8)) {
     y = base64encode(x, size=i)
     z = base64decode(y, typeof(x), size=i)
     stopifnot(mean(abs(x-z))<1e-5)
   }
   print("Checked base64 for encode/decode double type")
   
   x = log(as.complex(-(1:10)*pi))        # complex
   y = base64encode(x)
   z = base64decode(y, typeof(x))
   stopifnot(x==z)
   print("Checked base64 for encode/decode complex type")
  
   x = "Chance favors the prepared mind" # character
   y = base64encode(x)
   z = base64decode(y, typeof(x))
   stopifnot(x==z)
   print("Checked base64 for encode/decode character type")



