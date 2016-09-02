# q1
library(AppliedPredictiveModeling); library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis,predictors)
trainIndex <- createDataPartition(diagnosis, p = 0.50,list=F)
training <- adData[trainIndex,]
testing <- adData[-trainIndex,]

# q2
library(Hmisc)
data(concrete)
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, 
                              p = 3/4)[[1]]
training <- mixtures[ inTrain,]
testing <- mixtures[-inTrain,]

plot(training$CompressiveStrength)

ageFactor <- cut2(training$Age, quantile(training$Age))
FlyAshFactor <- cut2(training$FlyAsh, 
                     unique(quantile(training$FlyAsh)))

plot(training$CompressiveStrength, col = ageFactor)
qplot(training$CompressiveStrength, row.names(training), 
      col = ageFactor)

plot(training$CompressiveStrength, col = FlyAshFactor)
qplot(training$CompressiveStrength, row.names(training), 
      col = FlyAshFactor)

# q3
set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength, 
                               p = 3/4)[[1]]
training <- mixtures[ inTrain,]
testing <- mixtures[-inTrain,]

hist(training$Superplasticizer)
str(training$Superplasticizer)
head(sort(training$Superplasticizer), 50)
which(sort(training$Superplasticizer) > 0)[1]

# q4
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

idx <- which(substr(names(training), 1, 2) == 'IL')
names(training)[idx]
newdata <- training[, idx]
head(newdata)
preObj <- preProcess(newdata, method = 'pca', thresh = 0.9)
preObj$rotation

# q5
library(AppliedPredictiveModeling); library(caret)
data(AlzheimerDisease)
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

idx <- which(substr(names(training), 1, 2) == 'IL')
newdata <- training[, c(1, idx)]

# first approach
modelFit <- train(diagnosis ~ ., data = newdata, 
                  method = 'glm')
modelFit
modelFit$results[2]
predictions <- predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$diagnosis)

# using PCA
modelFit2 <- train(diagnosis ~ ., method = 'glm', 
                   preProcess = 'pca', data = newdata, 
                   trControl = trainControl(preProcOptions = list(thresh = 0.8)))
modelFit2
confusionMatrix(testing$diagnosis, predict(modelFit2, testing))

# find best treshold
results <- numeric(0)
models <- list()
for(i in seq(0.1, 0.9, by = 0.1)){
    testModel <- train(diagnosis ~ ., method = 'glm', 
                       preProcess = 'pca', data = newdata, 
                       trControl = trainControl(preProcOptions = list(thresh = i)))
    models <- append(models, testModel)
    results <- append(results, as.numeric(testModel$results[2]))
}
results
which(results == max(results))
plot(seq(0.1, 0.9, by = 0.1), results, xlab = 'threshold for PCA', ylab = 'training set error')
names(models[2])

# closer look at principal components
pc <- prcomp(training[, idx], center = T, scale = T)
plot(pc$sdev^2 / sum(pc$sdev^2))

# test model with threshold 0.2
testModel <- train(diagnosis ~ ., method = 'glm', 
                   preProcess = 'pca', data = newdata, 
                   trControl = trainControl(preProcOptions = list(thresh = i)))
confusionMatrix(testing$diagnosis, predict(testModel, testing))

