        ### SPAM example ###
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = .75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

    # first look
# capitalAve variable is very skewed and variable
hist(training$capitalAve, main = "", 
     xlab = "ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)
    
    # standarizing - center and scale
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
# standarizing test set with parameters from training set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)
# using preProcess function
preObj <- preProcess(training[ ,-58], method = c("center", "scale"))

trainCapAves <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)

testCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# passing preProcess as an argument
set.seed(32343)
modelFit <- train(type ~ ., data = training,
                  preProcess = c("center", "scale"), 
                  method = "glm")
modelFit

    # standarising - box-cox transforms
# continuous data -> more normal-like data
preObj <- preProcess(training[ ,-58], method = c("BoxCox"))
trainCapAves <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1, 2))
hist(trainCapAveS); qqnorm(trainCapAveS)

    # standarasing - imputing data
library(RANN)
set.seed(13343)
# make some values NA
training$CapAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$CapAve[selectNA] <- NA
# impute and standarize
preObj <- preProcess(training[, -58], method = 'knnImpute')
CapAve <- predict(preObj, training[, -58])$CapAve
# standarise true vals
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)
# compare
quantile(CapAve - capAveTruth)
quantile(CapAve - capAveTruth)[selectNA]
quantile(CapAve - capAveTruth)[!selectNA]
