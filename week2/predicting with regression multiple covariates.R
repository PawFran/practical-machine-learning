        ### example: wage data ###
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

    # Get training/test sets
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

    # feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage, plot="pairs")

    # plot age vs wage
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

    # linear model
modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

    # diagnostics
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

    # color by variables not used in the model
qplot(finMod$fitted, finMod$residuals, 
      colour = race, data = training)

    # plot by index
# if there is a trend here than probably sth variable
# is missing in the model - ex. time, age
plot(finMod$residuals,pch=19)

    # Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

    # If you want to use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)
