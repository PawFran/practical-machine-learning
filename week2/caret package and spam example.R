library(caret); library(kernlab); data(spam)
# creating training and testing set
inTrain <- createDataPartition(y = spam$type, p = .75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# fittin a model
set.seed(32343)
names(getModelInfo())
modelFit <- train(type ~ ., data = training, method = 'glm')
modelFit
modelFit$finalModel

# prediction
predictions <- predict(modelFit, newdata = testing)
predictions

# confusion matrix
?confusionMatrix(predictions, testing$type)


