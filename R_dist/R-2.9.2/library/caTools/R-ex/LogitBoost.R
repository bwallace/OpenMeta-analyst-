### Name: LogitBoost
### Title: LogitBoost Classification Algorithm
### Aliases: LogitBoost
### Keywords: classif

### ** Examples

  data(iris)
  Data  = iris[,-5]
  Label = iris[, 5]
  
  # basic interface
  model = LogitBoost(Data, Label, nIter=20)
  Lab   = predict(model, Data)
  Prob  = predict(model, Data, type="raw")
  t     = cbind(Lab, Prob)
  t[1:10, ]

  # two alternative call syntax
  p=predict(model,Data)
  q=predict.LogitBoost(model,Data)
  pp=p[!is.na(p)]; qq=q[!is.na(q)]
  stopifnot(pp == qq)

  # accuracy increases with nIter (at least for train set)
  table(predict(model, Data, nIter= 2), Label)
  table(predict(model, Data, nIter=10), Label)
  table(predict(model, Data),           Label)
  
  # example of spliting the data into train and test set
  mask = sample.split(Label)
  model = LogitBoost(Data[mask,], Label[mask], nIter=10)
  table(predict(model, Data[!mask,], nIter=2), Label[!mask])
  table(predict(model, Data[!mask,]),          Label[!mask])



