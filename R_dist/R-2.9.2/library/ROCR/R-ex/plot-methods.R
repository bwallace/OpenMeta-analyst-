### Name: plot-methods
### Title: Plot method for performance objects
### Aliases: plot.performance plot-methods plot,performance-method
###   plot,performance,missing-method
### Keywords: hplot

### ** Examples

# plotting a ROC curve:
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance( pred, "tpr", "fpr" )
plot( perf )

# To entertain your children, make your plots nicer
# using ROCR's flexible parameter passing mechanisms
# (much cheaper than a finger painting set)
par(bg="lightblue", mai=c(1.2,1.5,1,1))
plot(perf, main="ROCR fingerpainting toolkit", colorize=TRUE,
  xlab="Mary's axis", ylab="", box.lty=7, box.lwd=5,
  box.col="gold", lwd=17, colorkey.relwidth=0.5, xaxis.cex.axis=2,
  xaxis.col='blue', xaxis.col.axis="blue", yaxis.col='green', yaxis.cex.axis=2,
  yaxis.at=c(0,0.5,0.8,0.85,0.9,1), yaxis.las=1, xaxis.lwd=2, yaxis.lwd=3,
  yaxis.col.axis="orange", cex.lab=2, cex.main=2)



