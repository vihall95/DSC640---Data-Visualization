# DSC640
# Victoria Hall
# Exercise 1.2
# 3.27.2022


library(tidyverse)
library(readxl)
library(dplyr)

#Setting working directory
getwd()
setwd("C:/Users/hallt/Documents/DSC640 - Data Visualization")


#Reading in data
obama_df <- read_excel('obama-approval-ratings.xls') 
obama_df

#barplot
library(ggplot2)
p <- ggplot(data=obama_df, aes(x=Issue, y = Approve))+geom_bar(stat="identity", width=0.2,color="blue")
p <- p+theme(axis.text.x = element_text(angle=90))+ggtitle("Obama Approval Rating by Issue")
p


#Reading in data
hotdog_df<- read_excel('hotdog-contest-winners.xlsm') 
hotdog_df

#modifying table for graphing
hotdog_df1 <- hotdog_df %>% group_by(Country)
hotdog_df1 <- hotdog_df1 %>% summarise(hotdogs = sum(`Dogs eaten`),records=sum(`New record`))
Winner <- hotdog_df %>% group_by(Country) %>% summarise(Winner = n())
hotdog_df1
#pie chart
# Compute the position of labels
hotdog_df1 <- hotdog_df1 %>% 
  arrange(desc(Country)) %>%
  mutate(prop = hotdog_df1$hotdogs / sum(hotdog_df1$hotdogs) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
pie <- ggplot(hotdog_df1, aes(x="", y=prop, fill=Country)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Country), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")+ggtitle('Hot Dogs Eaten by Country')

pie

#Stacked Bar Chart
#Creating new column for dataset
vec = c('Other','Other','North America','North America')
hotdog_df1$Continent <- vec

hotdog_df1

stack <- ggplot(hotdog_df1, aes(x=Continent,y=hotdogs,fill = Country ))+geom_bar(stat= "identity")+ggtitle('Hotdogs Eaten by Continent')
stack


#Donut Chart
hotdog_df1$fraction = hotdog_df1$hotdogs / sum(hotdog_df1$hotdogs)

hotdog_df1$ymax = cumsum(hotdog_df1$fraction)

hotdog_df1$ymin = c(0, head(hotdog_df1$ymax, n=-1))

donut <- ggplot(hotdog_df1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) + ggtitle("Hotdogs Eaten By Country")
donut
  