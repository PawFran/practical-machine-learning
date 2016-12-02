data(iris); library(ggplot2); library(caret)
names(iris)

table(iris$Species)

# creating training and testing sets
inTrain <- createDataPartition(y = iris$Species, 
                               p = 0.7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training); dim(testing)

# plotting
qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

# fitting model
modFit <- train(Species ~ ., method = 'rpart', data = training)
modFit$finalModel

# plotting
plot(modFit$finalModel, uniform = TRUE, 
     main = "Classification Ttree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)

# prettier plot
library(rattle)
fancyRpartPlot(modFit$finalModel)

# prediction
prediction <- predict(modFit, newdata = testing)
sum(prediction == testing$Species) / length(prediction)
