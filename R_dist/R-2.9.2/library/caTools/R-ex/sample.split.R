### Name: sample.split
### Title: Split Data into Test and Train Set
### Aliases: sample.split
### Keywords: classif

### ** Examples

  library(MASS)
  data(cats)   # load cats data
  Y = cats[,1] # extract labels from the data
  msk = sample.split(Y, SplitRatio=3/4)
  table(Y,msk)
  t=sum( msk)  # number of elements in one class
  f=sum(!msk)  # number of elements in the other class
  stopifnot( round((t+f)*3/4) == t ) # test ratios
  
  # example of using group variable
  g = rep(seq(length(Y)/4), each=4); g[48]=12;
  msk = sample.split(Y, SplitRatio=1/2, group=g)
  table(Y,msk) # try to get correct split ratios ...
  split(msk,g) # ... while keeping samples with the same group label together

  # test results
  print(paste( "All Labels numbers: total=",t+f,", train=",t,", test=",f,
        ", ratio=", t/(t+f) ) )
  U = unique(Y)       # extract all unique labels
  for( i in 1:length(U)) {  # check for all labels
    lab = (Y==U[i])   # mask elements that have label U[i]
    t=sum( msk[lab])  # number of elements with label U[i] in one class
    f=sum(!msk[lab])  # number of elements with label U[i] in the other class 
    print(paste( "Label",U[i],"numbers: total=",t+f,", train=",t,", test=",f, 
                 ", ratio=", t/(t+f) ) )
  }
  
  # use results
  train = cats[ msk,2:3]  # use output of sample.split to ...
  test  = cats[!msk,2:3]  # create train and test subsets
  z = lda(train, Y[msk])  # perform classification
  table(predict(z, test)$class, Y[!msk]) # predicted & true labels
  
  # see also LogitBoost example



