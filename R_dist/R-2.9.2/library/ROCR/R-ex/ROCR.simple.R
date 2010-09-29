### Name: ROCR.simple
### Title: Data set: Simple artificial prediction data for use with ROCR
### Aliases: ROCR.simple
### Keywords: datasets

### ** Examples

# plot a ROC curve for a single prediction run
# and color the curve according to cutoff.
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)



