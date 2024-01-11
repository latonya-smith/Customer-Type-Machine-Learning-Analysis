#divide training and testing set
library(caret)
library(qacReg)
set.seed(1234)
index = createDataPartition(customer2$Accepted_Cmp, p=.8, list=FALSE)
train = customer2[index,]
test = customer2[-index,]
train_result=train

#set train_control
tc = trainControl(method = "cv", number = 10,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE)

#knn----------
set.seed(1234)
model_knn=train(Accepted_Cmp ~ . , 
                train, 
                method = "knn",
                metric = "ROC",
                trControl = tc, 
                tuneLength = 10)

model_knn

train_result$pred=predict(model_knn, train_result, type="prob")[[2]]
roc_plot(train_result$Accepted_Cmp, train_result$pred)
train_result$pred2=factor(train_result$pred > 0.33, 
                          levels = c(FALSE, TRUE),
                          labels = c("No", "Yes"))
confusionMatrix(train_result$pred2, train_result$Accepted_Cmp, positive = "Yes")
train_result$pred=NULL
train_result$pred2=NULL

#logistic---------
set.seed(1234)
model_lr <- train(Accepted_Cmp ~ .,
                  data = train,
                  method = "glm",
                  family = "binomial",
                  metric = "ROC",
                  trControl = tc)
model_lr
train_result$pred=predict(model_lr, train_result, type="prob")[[2]]
roc_plot(train_result$Accepted_Cmp, train_result$pred)
train_result$pred2=factor(train_result$pred > 0.28, 
                          levels = c(FALSE, TRUE),
                          labels = c("No", "Yes"))
confusionMatrix(train_result$pred2, train_result$Accepted_Cmp, positive = "Yes")
train_result$pred=NULL
train_result$pred2=NULL

#Logistic Regression with backward stepwise selection---------
set.seed(1234)
model_lrsw <- train(Accepted_Cmp ~ .,
                     data = train,
                     method = "glmStepAIC",
                     family = "binomial",
                     metric = "ROC",
                     trControl = tc)
model_lrsw
train_result$pred=predict(model_lrsw, train_result, type="prob")[[2]]
roc_plot(train_result$Accepted_Cmp, train_result$pred)
train_result$pred2=factor(train_result$pred > 0.28, 
                          levels = c(FALSE, TRUE),
                          labels = c("No", "Yes"))
confusionMatrix(train_result$pred2, train_result$Accepted_Cmp, positive = "Yes")
train_result$pred=NULL
train_result$pred2=NULL

#Logistic Regression with regularization-----------
set.seed(1234)
model_lrreg <- train(Accepted_Cmp ~ .,
                     data = train,
                     method = "glmnet",
                     family = "binomial",
                     metric = "ROC",
                     tuneLength = 10,
                     trControl = tc)
model_lrreg
train_result$pred=predict(model_lrreg, train_result, type="prob")[[2]]
roc_plot(train_result$Accepted_Cmp, train_result$pred)
train_result$pred2=factor(train_result$pred > 0.28, 
                          levels = c(FALSE, TRUE),
                          labels = c("No", "Yes"))
confusionMatrix(train_result$pred2, train_result$Accepted_Cmp, positive = "Yes")
train_result$pred=NULL
train_result$pred2=NULL