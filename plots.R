#import packages----
library(ggplot2)

#3 vars plot-------
customer2=customer
customer2$response=ifelse(customer2$Accepted_Cmp=="Yes", 1, 0)

#response ~ marital status + children
ggplot(data=customer2)+
  stat_summary(aes(x=Marital_Status, fill=children, y=response),
               fun="mean", geom="bar", position="dodge")+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(breaks = seq(0.0, 0.6, 0.1))+
  labs(x="Marital Status", 
       y="Proportion of Accepting Promotion", 
       title="Accepting Promotion and Marital Status by Children")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank()
  )

#remove one outlier
ggplot(customer2, aes(x = age, y= Income, color = Accepted_Cmp)) +
  geom_point()+
  geom_smooth()+
  labs(x="Age", color="Accept Promotion", 
       title="Relationship between Age and Income")+
  scale_y_continuous(breaks = seq(0, 180000, 30000))+
  scale_x_continuous(breaks=seq(20, 100, 10))+
  scale_color_manual(values=c("lightskyblue", "rosybrown2"))+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )


ggplot(customer2, aes(x = reorder(Education, desc(Income)), y= Income, fill = Accepted_Cmp)) +
  geom_bar(stat="summary", position = "dodge", fun="mean")+
  scale_y_continuous(breaks = seq(0, 65000, 5000))+
  labs(x="Education Level", 
       title="Relationship between Income and Education Level", 
       fill="Accept Promotion")+
  scale_fill_manual(values=c("lightskyblue", "rosybrown2"))+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )

str(customer2)
customer2$Complain=factor(customer2$Complain)


customer2$item_preference=factor(customer2$item_preference)
ggplot(data=customer2)+                           
  stat_summary(aes(x=children, y=response, fill=item_preference), 
               fun="mean", geom="bar", position = "dodge")+
  labs(x="Number of children at Home", 
       y="Proportion of Accepting Promotion", 
       title = "Relationship between Number of Children and Accepting Promotion by Item Preference", 
       fill="Item Preference")+
  scale_fill_brewer(palette = "Set2")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )

ggplot(data=customer2)+                           
  stat_summary(aes(x=Marital_Status, y=response, fill=item_preference), 
               fun="mean", geom="bar", position = "dodge")+
  labs(x="Marital Status", 
       y="Proportion of Accepting Promotion", 
       title = "Relationship between Marital Status and Accepting Promotion by Item Preference", 
       fill="Item Preference")+
  scale_fill_brewer(palette = "Set2")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )


ggplot(data=customer2)+                           
  geom_bar(aes(x=children, y=Income, fill=Accepted_Cmp), position = "dodge", stat = "summary", 
           fun="mean")+
  scale_y_continuous(breaks = seq(0, 70000, 10000))+
  labs(x="Number of children at Home", 
       y="Income", 
       title = "Relationship between Number of Children and Income by Accepting Promotion", 
       fill="Accept Promotion")+
  scale_fill_brewer(palette = "Set3")+
  theme(panel.background = element_rect(fill = "white"), #remove all lines
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color="black")
  )
