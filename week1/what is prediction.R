# spam example
library(kernlab)
data(spam)
head(spam)
names(spam)

with(spam, plot(density(your[type == 'nonspam']), col = 'blue', 
          main = '', xlab = 'frequency of "your"'))
with(spam, lines(density(your[type == 'spam']), col = 'red'))
# set a treshold for SPAM
abline(v = .5, col = 'black')

prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
result <- table(prediction, spam$type) / length(spam$type)
result
library(psych)
tr(result) # accuracy
