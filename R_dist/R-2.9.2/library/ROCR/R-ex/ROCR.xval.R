### Name: ROCR.xval
### Title: Data set: Artificial cross-validation data for use with ROCR
### Aliases: ROCR.xval
### Keywords: datasets

### ** Examples

# plot ROC curves for several cross-validation runs (dotted
# in grey), overlaid by the vertical average curve and boxplots
# showing the vertical spread around the average.
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="grey82",lty=3)
plot(perf,lwd=3,avg="vertical",spread.estimate="boxplot",add=TRUE)



