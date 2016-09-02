            ### wages example ###
library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(y = Wage$wage, 
                               p = .7, list = FALSE)
training <- Wage[inTrain, ]; testing <- Wage[-inTrain, ]

    # dummy variables
table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

    # removing zero covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

    # splines
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis
# fitting curve
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = .5)
points(training$age, predict(lm1, newdata = training), 
       col = 'red', pch = 19, cex = .5)
# splines on the test set
predict(bsBasis, age = testing$age)
