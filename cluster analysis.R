library(readr)
library(tidyverse)
library(dplyr)
library(readr)
library(qacBase)
library(descr)
library(factoextra)
library(cluster)
library(qacr)

### Cluster Analysis of Observed Data for Customer Types for people who accepted promotion -----------

managed_data<- read_csv("QAC385-Project/managed_data.csv")
managed_data<- managed_data %>%
 mutate(HighestPurchaseSource = case_when(
  NumWebPurchases >= NumCatalogPurchases & NumWebPurchases >= NumStorePurchases ~ "web",
  NumCatalogPurchases > NumWebPurchases & NumCatalogPurchases >= NumStorePurchases ~ "catalog",
  NumStorePurchases > NumWebPurchases & NumStorePurchases > NumCatalogPurchases ~ "store",
  TRUE ~ "equal"  # Handling cases where all three are equal
 ))

managed_data_promotionsY<- managed_data %>%
 filter(Accepted_Cmp == "Yes") 



data_vbls<- managed_data_promotionsY [c("Income", "MntWines", "MntFruits", 
                                          "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds", 
                                          "duration", "age", "children")]
data_vbls<- scale(data_vbls)
fviz_nbclust(data_vbls, kmeans, method = "wss") #within sum squares
cust_type1 = kmeans(data_vbls, centers = 3, nstart = 100)
managed_data_promotionsY$class<- cust_type1$cluster
data_vbls=as.data.frame(data_vbls)
x=cust_type1$cluster
data_vbls$type=x

profile_plot(data_vbls, cluster="type", type="bar")+
 scale_fill_brewer(palette = "Set3")


#### Cluster Analysis of Predicted Data for Customer Types for people who accepted promotion -----------
test_data<- read_csv("QAC385-Project/test_data.csv")
test_data<- test_data %>%
 mutate(HighestPurchaseSource = case_when(
  NumWebPurchases >= NumCatalogPurchases & NumWebPurchases >= NumStorePurchases ~ "web",
  NumCatalogPurchases > NumWebPurchases & NumCatalogPurchases >= NumStorePurchases ~ "catalog",
  NumStorePurchases > NumWebPurchases & NumStorePurchases > NumCatalogPurchases ~ "store",
  TRUE ~ "equal"  # Handling cases where all three are equal
 ))

test_data_promotionsY<- test_data %>%
 filter(pred == "Yes") 

 

data_variables<- test_data_promotionsY [c("Income", "MntWines", "MntFruits", 
                           "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds", 
                           "duration", "age", "children")]
data_variables=scale(data_variables)
fviz_nbclust(data_variables, kmeans, method = "wss") #within sum squares
cust_type = kmeans(data_variables, centers = 3, nstart = 100)
test_data_promotionsY$class<- cust_type$cluster
data_variables=as.data.frame(data_variables)
x=cust_type$cluster
data_variables$type=x

profile_plot(data_variables, cluster="type", type="bar")+
 scale_fill_brewer(palette = "Set3")

#Latent Variable Analysis
library(poLCA)

# Assuming 'data' is your data frame with categorical variables
# 'cat_vars' is a vector containing the names of categorical variables

# Perform latent class analysis
test_data_promotionsY$class<- as.factor(test_data_promotionsY$class)
test_data_promotionsY$Education<- as.factor(test_data_promotionsY$Education)
test_data_promotionsY$Marital_Status<- as.factor(test_data_promotionsY$Marital_Status)
test_data_promotionsY$HighestPurchaseSource<- as.factor(test_data_promotionsY$HighestPurchaseSource)

data_cat<- test_data_promotionsY[c("Education", "Marital_Status", "HighestPurchaseSource")]
data_cat<- as.data.frame(lapply(data_cat, as.factor))
data_cat$class<- factor(data_cat$class, ordered = FALSE)
contents(data_cat)
data_cat2<- as.formula(data_cat)
lca_model <- poLCA(data_cat2, data = data_cat, nclass=3)


class_probs <- lca_model$predclass
item_probs <- lca_result$probability






