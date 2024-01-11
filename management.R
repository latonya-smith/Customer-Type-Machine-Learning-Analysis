#import packages-------
library(tidyverse)
library(dplyr)
library(readr)
library(qacBase)
library(descr)
library(factoextra)
library(cluster)
library(qacr)
#edit variables------
customer=read_csv("/Users/liusimin/Desktop/QAC385/QAC385-Project/customer_personality.csv")

#remove NA
customer=na.omit(customer)

#merge Alone, YOLO and Absurd into the Single column
freq(customer$Marital_Status)
customer$Marital_Status[customer$Marital_Status %in% c("Alone", "Absurd", "YOLO")]="Single"

# Education variable we will recode: 2n Cycle to Technical School, 
# Basic to High School and below and Graduation to College Graduate.
freq(customer$Education)
customer$Education[customer$Education=="2n Cycle"]="Technical School"
customer$Education[customer$Education=="Basic"]="High School"
customer$Education[customer$Education=="Graduation"]="College Graduate"

# Create a variable called Age using the customer birth year(Year_Birth)
customer$age=2023-customer$Year_Birth
# 3 outliers greater than 100; delete
customer=customer %>% filter(age<100)

#Accepted_Cmp from AcceptedCmpt1, AcceptedCmpt2, AcceptedCmpt3, AcceptedCmpt4, AcceptedCmpt5 and Response 
customer=customer %>% mutate(Accepted_Cmp=AcceptedCmp3+AcceptedCmp1+AcceptedCmp2+
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
customer$children=factor(customer$children)
customer$Complain=factor(customer$Complain)

#Remove redundant info
delete=c("ID", "Year_Birth", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", 
         "AcceptedCmp1", "AcceptedCmp2", "Z_CostContact", "Z_Revenue", "Response")
customer=customer %>% select(-delete)

#Remove outliers
customer=customer %>% filter(Income < 200000)


#apply k-means clustering by customers' purchase preference (frequently bought)-----------
# PROBLEM:
# clusters don't depend on item-preference. Correlations not strong enough. 
# item_data=customer[c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")]
# item_data=scale(item_data)
# fviz_nbclust(item_data, kmeans, method = "wss") #within sum squares
# item_preference = kmeans(item_data, centers = 3, nstart = 100)
# customer$item_preference=item_preference$cluster
# item_data=as.data.frame(item_data)
# x=item_preference$cluster
# item_data$preference=x
# 
# profile_plot(item_data, cluster="preference", type="bar")+
#   scale_fill_brewer(palette = "Set3")
# #delete useless variables
# customer2=customer %>% select(ID, Education, Marital_Status, Income, NumDealsPurchases, 
#                              NumWebPurchases, NumCatalogPurchases, NumStorePurchases, NumWebVisitsMonth, 
#                              Accepted_Cmp, Complain, age, children, item_preference)

# write_csv(customer, "/Users/liusimin/Desktop/QAC385/QAC385-Project/managed_data.csv")
