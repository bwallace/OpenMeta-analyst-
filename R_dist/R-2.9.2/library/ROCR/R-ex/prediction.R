### Name: prediction
### Title: Function to create prediction objects
### Aliases: prediction
### Keywords: classif

### ** Examples

# create a simple prediction object
library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)



