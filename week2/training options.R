        ### SPAM example ###
library(caret); library(kernlab); data(spam)
    # creating training and testing set
inTrain <- createDataPartition(y = spam$type, p = .75, 
                               list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

    # training model
modelFit <- train(type ~ ., data = training, 
                  method = "glm")
args(train.default)
# setting weights may help when you more obs of a certain type
args(trainControl)
