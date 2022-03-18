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

wholesaledata <- wholesaledata %>% 
  mutate(Region=as.factor(Region),Channel=as.factor(Channel))
wholesaledata<- tibble(wholesaledata)

wholesaledata$Region<- ifelse(wholesaledata$Region==1,"Lizbon",
       ifelse(wholesaledata$Region==2,"Oporto","Other Region"))
wholesaledata$Channel <- ifelse(wholesaledata$Channel==1,"Horeca","Retail")
# Splitting data in two group Train and Test
dim(wholesaledata)
(N <- nrow(wholesaledata))
target <- round(0.8*N)
train_g <- runif(N,0,1)<0.8
saledata_train <- wholesaledata[train_g,]
saledata_test <- wholesaledata[!train_g,]
saledata_train %>% summary()
dim(saledata_train)
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
