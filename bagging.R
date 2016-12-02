library(ElemStatLearn); data(ozone, package = 'ElemStatLearn')
ozone <- ozone[order(ozone$ozone), ]
head(ozone)

predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
