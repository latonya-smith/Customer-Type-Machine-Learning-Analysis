#models: random forest, classification trees, gbm

# load the data
library(readr)
customer<- read_csv("managed_data.csv")

customer$Complain <- factor(customer$Complain)
#sum(customer$Complain==0) #2193
#sum(customer$Complain==1) #20

customer$item_preference <- factor(customer$item_preference)
customer$ID <-NULL

# examine the data
library(qacBase)
df_plot(customer)


# create train and test
library(caret)
set.seed(1234)
index = createDataPartition(customer$Accepted_Cmp, p=.8, list=FALSE)
train = customer[index,]
test = customer[-index,]


#Build models

##---------------------------------------------##
## Grow Random Forest - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.rf <- train(Accepted_Cmp ~ ., 
                  data = train, 
                  method = "rf",
                  metric = "ROC",
                  tuneLength = 10,
                  trControl=trainControl(method="cv", number = 10,classProbs = TRUE,
                                         summaryFunction = twoClassSummary))

model.rf
# mtry:5    ROC: 0.8246305  Sens: 0.9223716  Spec: 0.4359269


##---------------------------------------------##
## Grow Classification Tree - ROC              ##
##---------------------------------------------##
trctrl <- trainControl(method="cv", number=10,
                       summaryFunction=twoClassSummary,
                       classProbs=TRUE)

set.seed(1234)
model.ctree <- train(Accepted_Cmp ~., 
                     data = train, 
                     method = "rpart",
                     trControl=trctrl,
                     metric = "ROC",
                     tuneLength = 10)
model.ctree
#cp           ROC        Sens       Spec     
#0.003099174  0.7653533  0.8788760  0.4277211


##---------------------------------------------##
## Gradient Boost Machine - ROC              ##
##---------------------------------------------##
set.seed(1234)
model.gbm <- train(Accepted_Cmp~., 
                   data = train, 
                   method = "gbm",
                   metric = "ROC",
                   tuneLength=10,
                   trControl=trainControl(method="cv", number = 10,classProbs = TRUE,
                                          summaryFunction = twoClassSummary))
model.gbm
# interaction depth: 9, n.trees: 150  ROC: 0.8208725  Sens: 0.9037609  Spec: 0.4914541


## compare models
results <- resamples(list(ctree = model.ctree,
                          rf = model.rf,
                          gbm = model.gbm))
summary(results)
bwplot(results) #rf seems to be the best? but we will also try gbm

#SMOTE (to see if predictions on train data increases [more balanced])
tcsmote <- trainControl(method = "cv", number = 10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        sampling = "smote")
set.seed(1234)
model.rf_smote <- train(Accepted_Cmp ~ .,
                          data = train,
                          method = "rf",
                          metric = "ROC",
                          tuneLength = 10,
                          trControl = tcsmote)
model.rf_smote
# mtry:9    ROC: 0.8210502  Sens: 0.8812258  Spec: 0.5041667
# not improve a lot, use original rf


## Examine the ROC curve
library(qacReg)
prob <- predict(model.rf, train, type="prob")[[2]]
roc_plot(train$Accepted_Cmp, prob)
# Based on the training data, we would predict a specificity of 0.98 and a sensitivity of 1 if we use a probability cutoff value of 0.24.


#evaluate on test data
test$Accepted_Cmp <- factor(test$Accepted_Cmp)
prob <- predict(model.rf, test, type = "prob")[[2]]
pred <- ifelse(prob > 0.24, 1, 0)
pred <- factor(pred, levels = c(1, 0), labels = c("Yes", "No"))
pred <- factor(pred, levels = levels(test$Accepted_Cmp))
confusionMatrix(pred, test$Accepted_Cmp, positive = "Yes")
#Sensitivity : 0.8000; Specificity : 0.6854; Accuracy : 0.7166

#--------------------------------------------------------------------
#TRY GBM

#SMOTE (to see if predictions on train data increases [more balanced])
tcsmote <- trainControl(method = "cv", number = 10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        sampling = "smote")
set.seed(1234)
model.gbm_smote <- train(Accepted_Cmp ~ .,
                        data = train,
                        method = "gbm",
                        metric = "ROC",
                        tuneLength = 10,
                        trControl = tcsmote)
model.gbm_smote
# interaction depth: 8, n.trees: 200  ROC: 0.8172475  Sens: 0.8820070  Spec: 0.5039966
#use the original gbm

## Examine the ROC curve
library(qacReg)
prob <- predict(model.gbm, train, type="prob")[[2]]
roc_plot(train$Accepted_Cmp, prob)
# Based on the training data, we would predict a specificity of 0.9 and a sensitivity of 0.9 if we use a probability cutoff value of 0.31.


#evaluate on test data
test$Accepted_Cmp <- factor(test$Accepted_Cmp)
prob <- predict(model.gbm, test, type = "prob")[[2]]
pred <- ifelse(prob > 0.31, 1, 0)
pred <- factor(pred, levels = c(1, 0), labels = c("Yes", "No"))
pred <- factor(pred, levels = levels(test$Accepted_Cmp))
confusionMatrix(pred, test$Accepted_Cmp, positive = "Yes")
#Sensitivity : 0.6083, Specificity : 0.8069, Accuracy : 0.7528

#Random forest is the best model
