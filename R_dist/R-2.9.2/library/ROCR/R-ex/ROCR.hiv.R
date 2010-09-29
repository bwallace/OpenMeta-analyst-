### Name: ROCR.hiv
### Title: Data set: Support vector machines and neural networks applied to
###   the prediction of HIV-1 coreceptor usage
### Aliases: ROCR.hiv
### Keywords: datasets

### ** Examples

data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
perf.svm <- performance(pred.svm, 'tpr', 'fpr')
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn, 'tpr', 'fpr')
plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of
HIV-1 coreceptor usage")
plot(perf.nn, lty=3, col="blue",add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue",
spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.6,0.6,c('SVM','NN'),col=c('red','blue'),lwd=3)



