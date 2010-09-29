### Name: colAUC
### Title: Column-wise Area Under ROC Curve (AUC)
### Aliases: colAUC
### Keywords: univar

### ** Examples

# Load MASS library with "cats" data set that have following columns: sex, body
# weight, hart weight. Calculate how good weights are in predicting sex of cats.
# 2 classes; 2 features; 144 samples
library(MASS); data(cats);
colAUC(cats[,2:3], cats[,1], plotROC=TRUE) 

# Load rpart library with "kyphosis" data set that records if kyphosis
# deformation was present after corrective surgery. Calculate how good age, 
# number and position of vertebrae are in predicting successful operation. 
# 2 classes; 3 features; 81 samples
library(rpart); data(kyphosis);
colAUC(kyphosis[,2:4], kyphosis[,1], plotROC=TRUE)

# Example of 3-class 4-feature 150-sample iris data
data(iris)
colAUC(iris[,-5], iris[,5], plotROC=TRUE)
cat("Total AUC: \n"); 
colMeans(colAUC(iris[,-5], iris[,5]))

# Test plots in case of data without column names
Iris = as.matrix(iris[,-5])
dim(Iris) = c(600,1)
dim(Iris) = c(150,4)
colAUC(Iris, iris[,5], plotROC=TRUE)

# Compare calAUC with other functions designed for similar purpose
auc = matrix(NA,12,3)
rownames(auc) = c("colAUC(alg='ROC')", "colAUC(alg='Wilcox')", "sum(rank)",
    "wilcox.test", "wilcox_test", "wilcox.exact", "roc.area", "AUC", 
    "performance", "ROC", "auROC", "rcorr.cens")
colnames(auc) = c("AUC(x)", "AUC(-x)", "AUC(x+noise)")
X = cbind(cats[,2], -cats[,2], cats[,2]+rnorm(nrow(cats)) )
y = ifelse(cats[,1]=='F',0,1)
for (i in 1:3) {
  x = X[,i]
  x1 = x[y==1]; n1 = length(x1);                 # prepare input data ...
  x2 = x[y==0]; n2 = length(x2);                 # ... into required format
  data = data.frame(x=x,y=factor(y))
  auc[1,i] = colAUC(x, y, alg="ROC") 
  auc[2,i] = colAUC(x, y, alg="Wilcox")
  r = rank(c(x1,x2))
  auc[3,i] = (sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2)
  auc[4,i] = wilcox.test(x1, x2, exact=0)$statistic / (n1*n2) 
  ## Not run: 
##D   if (require("coin"))
##D     auc[5,i] = statistic(wilcox_test(x~y, data=data)) / (n1*n2) 
##D   if (require("exactRankTests"))  
##D     auc[6,i] = wilcox.exact(x, y, exact=0)$statistic / (n1*n2) 
##D   if (require("verification"))
##D     auc[7,i] = roc.area(y, x)$A.tilda 
##D   if (require("ROC")) 
##D     auc[8,i] = AUC(rocdemo.sca(y, x, dxrule.sca))    
##D   if (require("ROCR")) 
##D     auc[9,i] = performance(prediction( x, y),"auc")@y.values[[1]]
##D   if (require("Epi"))   auc[10,i] = ROC(x,y,grid=0)$AUC
##D   if (require("limma")) auc[11,i] = auROC(y, x)
##D   if (require("Hmisc")) auc[12,i] = rcorr.cens(x, y)[1]
##D   
## End(Not run)
}
print(auc)
stopifnot(auc[1, ]==auc[2, ])   # results of 2 alg's in colAUC must be the same
stopifnot(auc[1,1]==auc[3,1])   # compare with wilcox.test results

# time trials
x = matrix(runif(100*1000),100,1000)
y = (runif(100)>0.5)
system.time(colAUC(x,y,alg="ROC"   ))
system.time(colAUC(x,y,alg="Wilcox"))



