setwd("C:/Users/ilteb/Desktop/Works/git_workspace/R_data_analysis/Wholesale customers")
library("data.table")
library(dplyr)
library(ggplot2)
library("psych")
library(tidyr)
library("corrplot")
library(broom)
library(class)

wholesaledata <- fread("Wholesale customers data.csv")
wholesaledata %>% head()
wholesaledata %>% str()
wholesaledata %>% summary()

wholesaledata<- tibble(wholesaledata)
wholesaledata$Region<- ifelse(wholesaledata$Region==1,"Lizbon",
                              ifelse(wholesaledata$Region==2,"Oporto","Other Region"))
wholesaledata$Channel <- ifelse(wholesaledata$Channel==1,"Horeca","Retail")

wholesaledata <- wholesaledata %>% 
  mutate(Region=as.factor(Region),Channel=as.factor(Channel))
# Splitting data in two group Train and Test
dim(wholesaledata)
(N <- nrow(wholesaledata))

target <- round(0.8*N)
train_g <- runif(N,0,1)<0.8
saledata_train <- wholesaledata[train_g,]
saledata_test <- wholesaledata[!train_g,]
saledata_train %>% ggplot(aes(x=Channel,fill=Region))+
  geom_bar()
# Table format of two categorical columns
saledata_train %>% select(Channel,Region) %>% table()
# Proportion of them
saledata_train %>% select(Channel,Region) %>% table() %>% prop.table()
# Bar plot which is created side by side to see categorical case

saledata_train %>% ggplot(aes(x=Channel,fill=Region))+
  geom_bar(position="dodge")+
  labs(title = "Bar chart of Channel and Region")+
theme(axis.text.x = element_text(angle=45,size=12,color="black",vjust = -0.05),
      axis.ticks.x = element_blank())
# Another bar plot which is based on Region
saledata_train %>% ggplot(aes(x=Region,fill=Channel))+
  geom_bar(position="dodge")+
  theme(axis.text.x = element_text(color="black"))
# Simple Barcharts
library("colorRamps")
saledata_train %>% ggplot(aes(x=Channel))+
  geom_bar(fill=cm.colors(2,alpha=0.7))
# Faceting these factor columns 
saledata_train %>% ggplot(aes(x=Channel))+
  geom_bar()+
  facet_grid(~Region)
# Exploring Numerical data with dotplot
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Fresh))+
  geom_dotplot(dotsize=0.1)
# On the other hand histogram would be great to see distribution of it
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Milk))+
  geom_histogram(bins=15)
# Density plot of the some columns
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Frozen))+
  geom_density()
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Grocery))+
  geom_density()
# Finally lets see their boxplots
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Detergents_Paper))+
  geom_density()
saledata_train %>% select_if(is.numeric) %>% ggplot(aes(x=Delicassen))+
  geom_density()
# As may seen all of the points are seems they are right skewed forms
# Transformation may help to normalize these colums or at least may help to
# understand or make some interpretation.
# As another view option, faceted histograms may help to view existence of
# any distribution of them by region or channel
saledata_train %>% ggplot(aes(x=Milk))+
  geom_histogram(bins=15)+
  facet_grid(~Region)
saledata_train %>% ggplot(aes(x=Milk))+
  geom_histogram(bins=15)+
  facet_grid(~Channel)
names(saledata_train)
saledata_train %>% ggplot(aes(x=Fresh,fill=Region))+
  geom_histogram(bins=15)+
  facet_grid(~Region)
  
saledata_train %>% ggplot(aes(x=Fresh,fill=Channel))+
  geom_histogram(bins=15)+
  facet_grid(~Channel)

# Created corrplots
corrplot(saledata_train %>%  select_if(is.numeric)%>% cor(), tl.col = "red", tl.srt = 60, bg = "orange",
         title = "\n\n Correlation Plot Of Wholesale Customers",
         type = "upper")

saledata_train %>%  select_if(is.numeric)%>% corPlot(cex = 0.5)
# Optionally only color form can be used such that
corrplot(abs(cor(saledata_train %>%  select_if(is.numeric))),
         method="color",
         col=hcl.colors(8,palette = "viridis"))

# Lets see what we got in grouped forms
# The mean of grouped form 
saledata_train %>% 
  group_by(Channel,Region) %>%
  summarize(across(everything(),mean))

# The median of grouped form
saledata_train %>% 
  group_by(Channel,Region) %>%
  summarize(across(everything(),median))
# Let see for each group distribution is there any existance of outlier 
# Or abnormality
train_nest_form<- saledata_train %>% group_by(Channel,Region) %>% nest()
train_nest_fo
