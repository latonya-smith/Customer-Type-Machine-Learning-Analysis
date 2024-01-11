customer2=customer
customer2$Accepted_Cmp=factor(customer2$Accepted_Cmp)
#Statistical tests--------------
#Accepted_Cmp ~ marital status -> sig
chisq.test(customer2$Marital_Status, customer2$Accepted_Cmp)

#Accepted_Cmp ~ children -> sig
chisq.test(customer2$children, customer2$Accepted_Cmp)

#Accepted_Cmp ~ item_preference -> sig
chisq.test(customer2$item_preference, customer2$Accepted_Cmp)

#Accepted_Cmp ~ education -> sig
chisq.test(customer2$Education, customer2$Accepted_Cmp)

#Accepted_Cmp ~ age  -> not 
aov1=aov(age~Accepted_Cmp, data=customer2)
summary(aov1)

#Accepted_Cmp ~ income -> sig
aov2=aov(Income~Accepted_Cmp, data=customer2)
summary(aov2)

#Accepted_Cmp ~ NumWebPurchases -> sig
aov3=aov(NumWebPurchases~Accepted_Cmp, data=customer2)
summary(aov3)

#Accepted_Cmp ~ NumCatalogPurchases -> sig
aov4=aov(NumCatalogPurchases~Accepted_Cmp, data=customer2)
summary(aov4)

#Accepted_Cmpn ~ NumStorePurchases -> sig
aov5=aov(NumStorePurchases~Accepted_Cmp, data=customer2)
summary(aov5)

#Accepted_Cmpn ~ NumWebVisitsMonth -> sig
aov6=aov(NumWebVisitsMonth~Accepted_Cmp, data=customer2)
summary(aov6)

