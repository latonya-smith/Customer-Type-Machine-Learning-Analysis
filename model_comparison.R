# load the data
library(readr)
customer<- read_csv("managed_data.csv")


# examine the data
library(qacBase)
df_plot(customer)

# create train and test
library(caret)
set.seed(1234)
index = createDataPartition(customer$Accepted_Cmp, p=.8, list=FALSE)
train = customer[index,]
test = customer[-index,]


tc = trainControl(method = "cv", number = 10,
                  summaryFunction = twoClassSummary,
                  classProbs = TRUE)

##---------------------------------------------##
## KNN - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.knn <- train(Accepted_Cmp ~ . , 
                data = train, 
                method = "knn",
                metric = "ROC",
                trControl = tc, 
                tuneLength = 10)

model.knn


##---------------------------------------------##
## Logistic Regression - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.lr <- train(Accepted_Cmp ~ .,
                  data = train,
                  method = "glm",
                  family = "binomial",
                  metric = "ROC",
                  trControl = tc)
model.lr

##---------------------------------------------##
## #Logistic Regression with backward stepwise selection - ROC              ##
##---------------------------------------------##

set.seed(1234)
model.lrsw <- train(Accepted_Cmp ~ .,
                    data = train,
                    method = "glmStepAIC",
                    family = "binomial",
                    metric = "ROC",
                    trControl = tc)
model.lrsw

##---------------------------------------------##
## #Logistic Regression with regularization - ROC              ##
##---------------------------------------------##

set.seed(1234)
model.lrreg <- train(Accepted_Cmp ~ .,
                     data = train,
                     method = "glmnet",
                     family = "binomial",
                     metric = "ROC",
                     tuneLength = 10,
                     trControl = tc)
model.lrreg

##---------------------------------------------##
## Grow Random Forest - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.rf <- train(Accepted_Cmp ~ ., 
                  data = train, 
                  method = "rf",
                  metric = "ROC",
                  tuneLength = 10,
                  trControl=tc)

model.rf

##---------------------------------------------##
## Grow Classification Tree - ROC              ##
##---------------------------------------------##

set.seed(1234)
model.ctree <- train(Accepted_Cmp ~., 
                     data = train, 
                     method = "rpart",
                     trControl=tc,
                     metric = "ROC",
                     tuneLength = 10)
model.ctree

##---------------------------------------------##
## Gradient Boost Machine - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.gbm <- train(Accepted_Cmp~., 
                   data = train, 
                   method = "gbm",
                   metric = "ROC",
                   tuneLength=10,
                   trControl=tc)
model.gbm

#Evaluation 
results <- resamples(list(knn = model.knn, 
                          lr = model.lr, 
                          lrsw = model.lrsw, 
                          lrreg = model.lrreg,
                          rf = model.rf,
                          ctree = model.ctree,
                          gbm = model.gbm))
summary(results)
bwplot(results)

## Examine the ROC curve
library(qacReg)
prob <- predict(model.rf, train, type="prob")[[2]]

pred=factor(predict(model.rf, train))
roc_plot(train$Accepted_Cmp, prob)
confusionMatrix(pred, factor(train$Accepted_Cmp), positive = "Yes")
# Based on the training data, we would predict a specificity of 0.98 and a sensitivity of 1 if we use a probability cutoff value of 0.31.

# #evaluate on test data
# test$Accepted_Cmp <- factor(test$Accepted_Cmp)
# prob <- predict(model.rf, test, type = "prob")[[2]]
# pred <- ifelse(prob > 0.31, 1, 0) ## adjust cutoff points
# pred <- factor(pred, levels = c(0, 1), labels = c("No", "Yes"))
# confusionMatrix(pred, test$Accepted_Cmp, positive = "Yes")

## Random Forest predicts the best

