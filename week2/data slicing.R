        ### SPAM example ###
library(caret); library(kernlab); data(spam)
    # creating training and testing set
inTrain <- createDataPartition(y = spam$type, p = .75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

    # K-fold cross validation
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, 
                     list = TRUE, returnTrain = TRUE)
sapply(folds, length)

# which samples appear in the first fold ?
folds[[1]] [1 : 10]

# now returns test set samples
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, 
                     list = TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]] [1 : 10]

    # resampling cross validation
set.seed(32323)
folds <- createResample(y = spam$type, times = 10, 
                     list = TRUE)
sapply(folds, length)
folds[[1]] [1 : 10]

    # time slices
set.seed(32323)
t <- 1:1000
folds <- createTimeSlices(y = t, initialWindow = 20, 
                          horizon = 10)
names(folds)
folds$train[[1]]
