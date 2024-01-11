#create Plots (histograms, univariate graphs, bar graphs) 
library(readr)
customer <- read_csv("managed_data.csv")
# customer$Complain <- factor(customer$Complain)
# customer$item_preference <- factor(customer$item_preference)
# customer$children <- factor(customer$children)

library(qacBase)
contents(customer)
histograms(customer) ## only want numeric variables

barcharts(customer) ## only want categorical variables

#Accep_Cmp ~ item_preference + age+ children+ marital status + Education + Income 
#             + NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumWebVisitsMonth


## education level & item_preference

library(ggplot2)
customer$item_preference <- factor(customer$item_preference)
customer$Education <- factor(customer$Education)

# set.seed(1234)
# d <- data.frame(
#   Education = sample(c("High School", "College Graduate", "Master", "Technical School", "PhD"),
#                      100, replace = TRUE),
#   ItemPreference = sample(c("1","2","3"), 100, replace = TRUE))

# clustered bar chart
ggplot(customer, aes(x = Education, fill = item_preference)) +
  geom_bar() + 
  #scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "red")) +
  labs(title = "Education Level vs. Item Preference")



## education level & Accep_Cmp
# customer$Accepted_Cmp <- factor(customer$Accepted_Cmp)
# set.seed(1234)
# d1 <- data.frame(
#   Education = sample(c("High School", "College Graduate", "Master", "Technical School", "PhD"),
#                      100, replace = TRUE),
#   AcceptedCmp = sample(c("No","Yes"), 100, replace = TRUE))

ggplot(customer, aes(x = Accepted_Cmp, fill = Education)) +
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("No" = "yellow", "Yes" = "purple")) +
  labs(title = "Education Level vs. Campaign Acceptance")


## item_preference & Accep_Cmp
# set.seed(1234)
# d2 <- data.frame(
#   ItemPreference = sample(c("1","2","3"), 100, replace = TRUE),
#   AcceptedCmp = sample(c("No","Yes"), 100, replace = TRUE))

ggplot(customer, aes(x = item_preference, fill = Accepted_Cmp)) +
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("No" = "blue", "Yes" = "orange")) +
  labs(title = "Item Preference vs. Campaign Acceptance")


## item_preference & marital status
#customer$Marital_Status <- factor(customer$Marital_Status)
# set.seed(1234)
# d3 <- data.frame(
#   ItemPreference = sample(c("1","2","3"), 100, replace = TRUE),
#   MaritalStatus = sample(c("Divorced","Married","Single","Together","Widow"), 100, replace = TRUE))

ggplot(customer, aes(x = item_preference, fill = Marital_Status)) + ## can also tweak around
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "red")) +
  labs(title = "Item Preference vs. Marital Status")


## Accep_Cmp & marital status
# set.seed(1234)
# d4 <- data.frame(
#   AcceptedCmp = sample(c("No","Yes"), 100, replace = TRUE),
#   MaritalStatus = sample(c("Divorced","Married","Single","Together","Widow"), 100, replace = TRUE))

ggplot(customer, aes(x = Accepted_Cmp, fill = Marital_Status)) + ## can also tweak around
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("No" = "blue", "Yes" = "orange")) +
  labs(title = "Campaign Acceptance vs. Marital Status")

## children & Accep_Cmp
#customer$children<- factor(customer$children)
# set.seed(1234)
# d5 <- data.frame(
#   AcceptedCmp = sample(c("No","Yes"), 100, replace = TRUE),
#   Numchild = sample(c("0","1","2","3"), 100, replace = TRUE))

ggplot(customer, aes(x = Accepted_Cmp, fill = children)) + ## can also tweak around
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("No" = "blue", "Yes" = "orange")) +
  labs(title = "Number of Children & Campaign Acceptance",
       y = "Number of children")

## children & item_preference
# set.seed(1234)
# d6 <- data.frame(
#   ItemPreference = sample(c("1","2","3"), 100, replace = TRUE),
#   Numchild = sample(c("0","1","2","3"), 100, replace = TRUE))

ggplot(customer, aes(x = item_preference, fill = children)) + ## can also tweak around
  geom_bar(position = "dodge") + 
  #scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "red")) +
  labs(title = "Number of Children vs. Item Preference",
       y = "Number of children")

# ## NumDealsPurchases & Accep_Cmp
# library(ggplot2)
# 
# set.seed(1234)
# d7 <- data.frame(
#   NumDealsPurchases = sample(0:15, 100, replace = TRUE),
#   AcceptedCmp = sample(c("No","Yes"), 100, replace = TRUE)
# )
# 
# ggplot(d7, aes(x = AcceptedCmp, y = NumDealsPurchases)) +
#   geom_bar(stat = "summary", fun = "sum", position = "dodge") +
#   labs(title = "Number of Deals vs. Campaign Acceptance",
#        x = "Accepted Campaign",
#        y = "Number of Deals")
#  
# ## NumDealsPurchases & item_preference
# set.seed(1234)
# d8 <- data.frame(
#   ItemPreference = sample(c("1","2","3"), 100, replace = TRUE),
#   NumDealsPurchases = sample(0:15, 100, replace = TRUE))
# 
# ggplot(d8, aes(x = ItemPreference, y = NumDealsPurchases)) +
#   geom_bar(stat = "summary", fun = "sum", position = "dodge") + 
#   labs(title = "Number of Deals vs. Item Preference",
#        x = "Item Preference",
#        y = "Number of Deals")

## Univariate Graphs
ggplot(customer, aes(x=Education)) + geom_bar()
ggplot(customer, aes(x=item_preference)) + geom_bar()
ggplot(customer, aes(x= Marital_Status)) + geom_bar()
ggplot(customer, aes(x= children)) + geom_bar()
ggplot(customer, aes(x= age)) + geom_bar()
ggplot(customer, aes(x= Accepted_Cmp)) + geom_bar()

library(dplyr)
income= customer %>% 
  filter(Income < 200000)
  
ggplot(data=income) + geom_histogram(aes(x=Income), fill="purple", color="white") +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  scale_x_continuous(breaks = seq(0, 200000, 20000))+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )




