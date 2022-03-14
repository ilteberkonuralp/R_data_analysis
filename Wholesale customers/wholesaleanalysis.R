library("data.table")
library(dplyr)
library(ggplot2)
library("psych")
library("corrplot")

wholesaledata <- fread("Wholesale customers data.csv")
wholesaledata %>% head()
wholesaledata %>% str()
# Created corrplots
corrplot(wholesaledata %>% cor(), tl.col = "red", tl.srt = 60, bg = "orange",
         title = "\n\n Correlation Plot Of Wholesale Customers",
         type = "upper")

wholesaledata %>% corPlot()
