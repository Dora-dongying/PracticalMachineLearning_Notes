library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

## Create partition
##inTrain <- createDataPartition(y = segmentationOriginal$Case, p = 0.6, 
##                                list = FALSE)
## Subset the data to a training set and testing set based on the Case variable in the data set.
data_train <- segmentationOriginal[segmentationOriginal$Case == "Train",]
data_test <- segmentationOriginal[segmentationOriginal$Case == "Test",]

## Set the seed to 125 and fit a CART model with the rpart method using all predictor variables 
## and default caret settings.
set.seed(125)
modFit <- train(Class ~., method = "rpart", data = data_train)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
library(rattle)
fancyRpartPlot(modFit$finalModel)

## olive data
library(pgmm)
data(olive)
olive = olive[,-1]

## Create Training Data
inTrain <- createDataPartition(y = olive$Area, p = 0.7, list = FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
modFit <- train(Area ~., method = "rpart", data = olive)
print(modFit$finalModel)
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata = as.data.frame(t(colMeans(olive))))

## South Africa Heart Disease

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
