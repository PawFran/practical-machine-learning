        ### SPAM example ###
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = .75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

    # correlated predictors
m <- abs(cor(training[, -58])) # 58th col is the outcome
diag(m) <- 0 # avery var is correlated to itself
which(m > 0.8, arr.ind = TRUE)

names(spam)[c(32, 34)]
plot(spam[, 34], spam[, 32])

    # rotating the plot
x <- .71 * training$num415 + .71 * training$num857
y <- .71 * training$num415 - .71 * training$num857
plot(x, y)

    # principal components
smallSpam <- spam[, c(32, 34)]
prComp <- prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])

prComp$rotation

    # PCA on SPAM
# spam is black and ham is red
typeColor <- ((spam$type == "spam") * 1 + 1)
# use log ex. when variables are skewed
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor, 
     xlab = "PC1", ylab = "PC2")

    # PCA with caret
# preProcess just computes a model
preProc <- preProcess(log10(spam[, -58] + 1), 
                      method = 'pca', pcaComp = 2)
# now apply data
spamPC <- predict(preProc, log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor)

    # preprocessing with PCA
preProc <- preProcess(log10(spam[, -58] + 1), 
                      method = 'pca', pcaComp = 2)
trainPC <- predict(preProc, log10(spam[, -58] + 1))
modelFit <- train(training$type ~ ., method = "glm", 
                  data = trainPC)
# must use same principal components with test set
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

    # Alternative (sets # of PCs)
modelFit <- train(training$type ~ .,method="glm", 
                  preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))
