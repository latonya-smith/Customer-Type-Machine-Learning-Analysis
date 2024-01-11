library(ggplot2)
library(readr)
library(dplyr)

data<- read_csv("managed_data_all.csv")

#Visualizing Education with Item Preference
data$Education <- as.factor(data$Education)
data$item_preference <- as.factor(data$item_preference)
ggplot(data, aes(x = Education, fill = item_preference)) +
 geom_bar(position = "dodge") +
 labs(title = "Item Preferences by Education", 
      y="Count", fill="Item Preference") +
 theme(axis.text.x = element_text(angle = 55, hjust = 1), 
       panel.background = element_rect(fill = "white"), #remove all lines
       axis.line = element_line(color="black"),
       strip.background = element_blank(),
       legend.key = element_rect(fill = "white", color="black")) + 
  scale_fill_brewer(palette = "Set2")


#Visualizing age and Item Preference and whether or not they accepted a promotion

ggplot(data, aes(x = item_preference, y = age, fill = Accepted_Cmp)) +
 geom_boxplot() +
 labs(title = "Age Across Item Preference and Campaign Acceptance") + 
 theme_minimal() 

# Visualizing age and whether or not they accepted any campaign

ggplot(data, aes(x = age, fill = Accepted_Cmp)) + geom_bar() + 
 labs(title = "Distribution of Accepting Campaigns Across Age") + 
 scale_fill_brewer(palette = "Set3")

# Visualizing whether or not Low income individuals accepted the campaigns and 
# if they would do so more if they had kids at home
ggplot(data, aes(x = Accepted_Cmp, y = Income)) +
 geom_boxplot(outlier.shape = NA) +  # Remove outliers
 facet_wrap(~as.factor(Kidhome)) +
 labs(title = "Box Plot of Income Across Accepted_Cmp") +
 theme_minimal()

data_lowIncome<- data %>%
 filter(Income <= 20000)

ggplot(data_lowIncome, aes(x = Accepted_Cmp, y = Income)) +
 geom_bar(stat = "summary", fun = "mean", fill = "lavender") +
 labs(title = "Distribution of Campaign Acceptance over Low Income Individuals",
      x = "Accepted_Cmp", y = "Mean Income") +
 theme_minimal()

ggplot(data_lowIncome, aes(x = Accepted_Cmp, y = Income, fill = Accepted_Cmp)) +
 geom_boxplot(outlier.shape = NA) +  # Remove outliers
 facet_wrap(~as.factor(Kidhome)) +
 labs(title = "Distribution of Low Income individuals with kids who accepted a Campaign") +
 scale_fill_brewer(palette = "Set1")

#Store purchases per month and whether or not they accepted a campaign
data_pur_pref<- data %>%
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
  scale_fill_brewer(palette = "Set3")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black"))

