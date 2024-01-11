# aggregate all code into one file
#import packages-------
library(tidyverse)
library(dplyr)
library(readr)
library(qacBase)
library(descr)
library(factoextra)
library(cluster)
library(qacr)
library(caret)
library(qacReg)
library(ggplot2)

##---------------------------------------------##
## Data Management              ##
##---------------------------------------------##
# load the data
library(readr)
customer=read_csv("customer_personality.csv")

#remove NA
customer=na.omit(customer)

#merge Alone, YOLO and Absurd into the Single column
customer$Marital_Status[customer$Marital_Status %in% c("Alone", "Absurd", "YOLO")]="Single"

# Education variable we will recode: 2n Cycle to Technical School, 
# Basic to High School and below and Graduation to College Graduate.
customer$Education[customer$Education=="2n Cycle"]="Technical School"
customer$Education[customer$Education=="Basic"]="High School"
customer$Education[customer$Education=="Graduation"]="College Graduate"

# Create a variable called Age using the customer birth year(Year_Birth)
customer$age=2023-customer$Year_Birth
# 3 outliers greater than 100; delete
library(dplyr)
customer=customer %>% filter(age<100)

#Accepted_Cmp from AcceptedCmpt1, AcceptedCmpt2, AcceptedCmpt3, AcceptedCmpt4, AcceptedCmpt5 and Response 
customer=customer %>% 
  mutate(Accepted_Cmp=AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+
                               AcceptedCmp4+AcceptedCmp5+Response) %>% 
  mutate(Accepted_Cmp=ifelse(Accepted_Cmp>0, "Yes", "No")) %>% 
  mutate(Accepted_Cmp=factor(Accepted_Cmp), Education=factor(Education), Marital_Status=factor(Marital_Status))

#Change Dt_Customer variable type from character to date.
customer$Dt_Customer=as.POSIXct(customer$Dt_Customer, format="%d-%m-%Y")

#combine Kidhome and Teenhome into one variable called children
customer$children=customer$Teenhome+customer$Kidhome

#Calculate the years of shopping here
customer$duration=as.numeric(difftime(Sys.Date(), customer$Dt_Customer, units = "days") / 365.25)

#Make factors
customer$Complain=factor(customer$Complain)

#Remove redundant info/useless variables
delete=c("ID", "Year_Birth", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", 
         "AcceptedCmp1", "AcceptedCmp2", "Z_CostContact", "Z_Revenue", "Response")
customer=customer %>% select(-delete)

#Remove outliers
customer=customer %>% filter(Income < 200000)

# data review
library(qacBase)
df_plot(train)

##------------------------------------------------------------------------------

##---------------------------------------------##
## Statistical tests              ##
##---------------------------------------------##

#Accepted_Cmp ~ marital status -> sig
chisq.test(customer$Marital_Status, customer$Accepted_Cmp)

#Accepted_Cmp ~ children -> sig
chisq.test(customer$children, customer$Accepted_Cmp)

#Accepted_Cmp ~ education -> sig
chisq.test(customer$Education, customer$Accepted_Cmp)

#Accepted_Cmp ~ marital status -> not
chisq.test(customer$Complain, customer$Accepted_Cmp)

#Accepted_Cmp ~ age  -> not 
aov1=aov(age~Accepted_Cmp, data=customer)
summary(aov1)

#Accepted_Cmp ~ income -> sig
aov2=aov(Income~Accepted_Cmp, data=customer)
summary(aov2)

#Accepted_Cmp ~ NumWebPurchases -> sig
aov3=aov(NumWebPurchases~Accepted_Cmp, data=customer)
summary(aov3)

#Accepted_Cmp ~ NumCatalogPurchases -> sig
aov4=aov(NumCatalogPurchases~Accepted_Cmp, data=customer)
summary(aov4)

#Accepted_Cmp ~ NumStorePurchases -> sig
aov5=aov(NumStorePurchases~Accepted_Cmp, data=customer)
summary(aov5)

#Accepted_Cmp ~ NumWebVisitsMonth -> sig
aov6=aov(NumWebVisitsMonth~Accepted_Cmp, data=customer)
summary(aov6)

#Accepted_Cmp ~ MntWines -> sig
aov7=aov(MntWines~Accepted_Cmp, data=customer)
summary(aov7)

#Accepted_Cmp ~ MntFruits -> sig
aov8=aov(MntFruits~Accepted_Cmp, data=customer)
summary(aov8)

#Accepted_Cmp ~ MntMeatProducts -> sig
aov9=aov(MntMeatProducts~Accepted_Cmp, data=customer)
summary(aov9)

#Accepted_Cmp ~ MntFishProducts -> sig
aov10=aov(MntFishProducts~Accepted_Cmp, data=customer)
summary(aov10)

#Accepted_Cmp ~ MntSweetProducts -> sig
aov11=aov(MntSweetProducts~Accepted_Cmp, data=customer)
summary(aov11)

#Accepted_Cmp ~ MntGoldProds -> sig
aov12=aov(MntGoldProds~Accepted_Cmp, data=customer)
summary(aov12)

#Accepted_Cmp ~ duration -> sig
aov13=aov(duration~Accepted_Cmp, data=customer)
summary(aov13)

##------------------------------------------------------------------------------
#Building Models

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
## KNN - ROC      
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
## Logistic Regression - ROC          
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
## Logistic Regression with backward stepwise selection - ROC  
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
## #Logistic Regression with regularization - ROC        
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
## Grow Random Forest - ROC           
##---------------------------------------------##
set.seed(1234)
model.rf <- train(Accepted_Cmp ~ ., 
                  data = train, 
                  method = "rf",
                  metric = "ROC",
                  tuneLength = 10,
                  varImp=TRUE,
                  trControl=tc)

model.rf

##---------------------------------------------##
## Grow Classification Tree - ROC             
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
## Gradient Boost Machine - ROC             
##---------------------------------------------##
set.seed(1234)
model.gbm <- train(Accepted_Cmp~., 
                   data = train, 
                   method = "gbm",
                   metric = "ROC",
                   tuneLength=10,
                   verbose = FALSE,
                   trControl=tc)
model.gbm

#Evaluation on training set--------
prob <- predict(model.rf, train, type="prob")[[2]]
pred <- factor(predict(model.rf, train))
confusionMatrix(pred, factor(train$Accepted_Cmp), positive = "Yes")

## Examine the ROC curve
library(qacReg)
roc_plot(train$Accepted_Cmp, prob)

## compare models
results <- resamples(list(knn = model.knn, 
                          lr = model.lr, 
                          lrsw = model.lrsw, 
                          lrreg = model.lrreg,
                          rf = model.rf,
                          ctree = model.ctree,
                          gbm = model.gbm))
summary(results)
bwplot(results)
#pick rf


#evaluate on test data
test$Accepted_Cmp <- factor(test$Accepted_Cmp)
prob <- predict(model.rf, test, type = "prob")[[2]]
pred <- ifelse(prob > 0.19, 1, 0)
pred <- factor(pred, levels = c(0, 1), labels = c("No", "Yes"))
confusionMatrix(pred, test$Accepted_Cmp, positive = "Yes")

#0.23: Sensitivity : 0.8500; Specificity : 0.7103; Accuracy : 0.7483

#importance of variables
varImp(model.rf)
plot(varImp(model.rf))





##------------------------------------------------------------------------------


##---------------------------------------------##
## Graphs (might not really be used)             
##---------------------------------------------##
#graphs: univariate----------
library(ggplot2)
color_values=rainbow(5)
ggplot(customer, aes(x=Education, fill=Education)) + geom_bar()+
  scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0, 1200))+
  labs(x="Education levels", 
       y="Count", 
       title="Distribution of Education")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )+
  scale_fill_manual(values = color_values)


ggplot(customer, aes(x= Marital_Status, fill=Marital_Status)) + geom_bar()+
  scale_y_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1000))+
  labs(x="Marital Status", 
       y="Count", 
       title="Distribution of Marital Status", 
       fill="Marital Status")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )+
  scale_fill_manual(values = color_values)

ggplot(customer, aes(x= Accepted_Cmp, fill=Accepted_Cmp)) + geom_bar()+
  scale_y_continuous(breaks = seq(0, 1800, 200), limits = c(0, 1800))+
  labs(x="Accept Promotion", 
       y="Count", 
       title="Distribution of Accepting Promotion", 
       fill="Accept Promotion")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )+
  scale_fill_manual(values = color_values)

#graphs: bivariate----------
data_pur_pref<- customer %>%
  mutate(HighestPurchaseSource = case_when(
    NumWebPurchases >= NumCatalogPurchases & NumWebPurchases >= NumStorePurchases ~ "web",
    NumCatalogPurchases > NumWebPurchases & NumCatalogPurchases >= NumStorePurchases ~ "catalog",
    NumStorePurchases > NumWebPurchases & NumStorePurchases > NumCatalogPurchases ~ "store",
    TRUE ~ "equal"  # Handling cases where all three are equal
  ))

ggplot(data_pur_pref, aes(x = Accepted_Cmp, fill = HighestPurchaseSource)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Purchasing Methods Across Campaign Acceptance",
       x = "Accept Promotion",
       y = "Count", 
       fill="Highest Purchase Source")+
  scale_fill_manual(values = color_values)+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12))

data_lowIncome = customer %>% filter(Income <= 20000)
ggplot(data_lowIncome, aes(x = Accepted_Cmp, y = Income, fill = Accepted_Cmp)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Distribution of Campaign Acceptance over Low Income Individuals (<= 20000)",
       x = "Accept Promotion", y = "Mean Income", fill="Accept Promotion")+
  scale_fill_manual(values = color_values)+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12))

#graphs: trivariate---------
customer2=customer
customer2$response=ifelse(customer2$Accepted_Cmp=="Yes", 1, 0)
ggplot(customer2, aes(x = reorder(Education, desc(Income)), y= Income, fill = Accepted_Cmp)) +
  geom_bar(stat="summary", position = "dodge", fun="mean")+
  scale_y_continuous(breaks = seq(0, 65000, 5000))+
  labs(x="Education Level", 
       title="Relationship between Income and Education Level", 
       fill="Accept Promotion")+
  scale_fill_manual(values=color_values)+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )

ggplot(customer2, aes(x = reorder(Accepted_Cmp, desc(Income)), y= Income, fill = Education)) +
  geom_bar(stat="summary", position = "dodge", fun="mean")+
  scale_y_continuous(breaks = seq(0, 65000, 5000))+
  labs(fill="Education Level", 
       title="Relationship between Income and Education Level", 
       x="Accept Promotion")+
  scale_fill_manual(values=color_values)+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )

ggplot(data=customer2)+
  stat_summary(aes(x=Marital_Status, fill=factor(children), y=response),
               fun="mean", geom="bar", position="dodge")+
  scale_y_continuous(breaks = seq(0.0, 0.6, 0.1))+
  labs(x="Marital Status", 
       y="Proportion of Accepting Promotion", 
       title="Accepting Promotion and Marital Status by Children", 
       fill="Number of Children")+
  scale_fill_manual(values=color_values)+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black", linewidth = 1.5),
        strip.background = element_blank(), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=14), 
        title = element_text(size=16),
        legend.text = element_text(size=12)
  )



