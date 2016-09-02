    ### spam and number of capital letters ###
library(kernlab); data(spam); set.seed(333)
# take ten random rows
smallSpam <- spam[sample(dim(spam)[1], size = 10), ]
# labels: 1 for spam and 2 for nonspam
spamLabel <- (smallSpam$type == 'spam') * 1 + 1
plot(smallSpam$capitalAve, col = spamLabel)

# prediction - rule1 (fits perfectly training set)
rule1 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.4] <- "nonspam"
    prediction[(x >= 2.4 & x < 2.45)] <- "spam"
    prediction[(x > 2.45 & x <= 2.7)] <- "nonspam"
    return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type)

# prediction rule2
rule2 <- function(x){
    prediction <- rep(NA, length(x))
    prediction[x > 2.8] <- "spam"
    prediction[x <= 2.8] <- "nonspam"
    return(prediction)
}
table(rule2(smallSpam$capitalAve), smallSpam$type)

# apply to complete spam
t1 <- table(rule1(spam$capitalAve), spam$type)
t2 <- table(rule2(spam$capitalAve), spam$type)
t1
t2
# look at accuracy
tr(t1)
tr(t2)
