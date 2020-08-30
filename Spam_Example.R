library(caret)
library(splines)
library(kernlab)
library(ggplot2)

data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

## Corelated predictore
M <- abs(cor(training[, -58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)