### Name: knn.cv
### Title: k-Nearest Neighbour Cross-Validatory Classification
### Aliases: knn.cv
### Keywords: classif

### ** Examples

train <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
cl <- factor(c(rep("s",50), rep("c",50), rep("v",50)))
knn.cv(train, cl, k = 3, prob = TRUE)
attributes(.Last.value)



