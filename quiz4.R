## Q1: vowel data
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

## Set the variable y to be a factor variable in both the training and test set. 
## Then set the seed to 33833. Fit (1) a random forest predictor relating the factor 
## variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. 
## Fit these both with the train() command in the caret package.
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modFit1 <- train(y ~., method = "rf", data = vowel.train)
modFit2 <- train(y ~., method = "gbm", data = vowel.train)
pred1 <- predict(modFit1, newdata = vowel.test)
pred2 <- predict(modFit2, newdata = vowel.test)

## What are the accuracies for the two approaches on the test data set? 
## What is the accuracy among the test set samples where the two methods agree?
confusionMatrix(pred1, vowel.test$y)$overall[1]
confusionMatrix(pred2, vowel.test$y)$overall[1]
predDF <- data.frame(pred1, pred2, y = vowel.test$y)
# Accuracy among the test set samples where the two methods agree
sum(pred1[predDF$pred1 == predDF$pred2] == 
            predDF$y[predDF$pred1 == predDF$pred2]) / 
        sum(predDF$pred1 == predDF$pred2)

## Q2: Alzheimer's data
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Set the seed to 62433 and predict diagnosis with all the other variables 
## using a random forest ("rf"), boosted trees ("gbm") and linear discriminant 
## analysis ("lda") model. Stack the predictions together using random forests ("rf"). 
## What is the resulting accuracy on the test set? Is it better or worse than each of 
## the individual predictions?
set.seed(62433)
modFit2_rf <- train(diagnosis ~., method = "rf", data = training)
modFit2_gbm <- train(diagnosis ~., method = "gbm", data = training)
modFit2_lda <- train(diagnosis ~., method = "lda", data = training)
pred2_rf <- predict(modFit2_rf, newdata = testing)
pred2_gbm <- predict(modFit2_gbm, newdata = testing)
pred2_lda <- predict(modFit2_lda, newdata = testing)
predDF2 <- data.frame(pred2_rf, pred2_gbm, pred2_lda, diagnosis = testing$diagnosis)
combModFit2 <- train(diagnosis ~., method = "rf", data = predDF2)
CombPred2 <- predict(combModFit2, newdata = predDF2)

## Check the accuracy
confusionMatrix(pred2_rf, testing$diagnosis)$overall[1]
confusionMatrix(pred2_gbm, testing$diagnosis)$overall[1]
confusionMatrix(pred2_lda, testing$diagnosis)$overall[1]
confusionMatrix(CombPred2, testing$diagnosis)$overall[1]

## Q3: Concrete Data
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

## Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
## Which variable is the last coefficient to be set to zero as the penalty increases? 
## (Hint: it may be useful to look up ?plot.enet).
set.seed(233)
modFit3 <- train(CompressiveStrength ~., method = "lasso", data = training)
pred3 <- predict(modFit3, newdata = testing)
library(elasticnet)
plot.enet(modFit3$finalModel, xvar = "penalty", use.color = TRUE)

## Q4: visitors
library(lubridate) # For year() function below
dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

## Fit a model using the bats() function in the forecast package to the training time series. 
## Then forecast this model for the remaining time points. For how many of the testing points 
## is the true value within the 95% prediction interval bounds?
library(forecast)
modFit4 <- bats(tstrain)
fcast4 <- forecast(modFit4, level = 95, h = dim(testing)[1])
sum(fcast4$lower < testing$visitsTumblr & testing$visitsTumblr < fcast4$upper) / 
        dim(testing)[1]

## Q5: concrete data again
set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)