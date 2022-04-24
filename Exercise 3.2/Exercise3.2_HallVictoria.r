# Week 5 and 6 Exercise
# Victoria Hall
# DSC640
# 4.21.2022


#loading libraries
library(tidyverse)
library(readxl)
library(treemapify)

#reading in approval rating data
approval_df<-read_excel("obama-approval-ratings.xls")

setwd("C:/Users/hallt/Documents/DSC640 - Data Visualization/Exercise 3.2")

approval_df

#Creating treemap
treemap <- ggplot(approval_df, aes(area=Approve,fill=Issue,label=Issue))+geom_treemap()
treemap <- treemap + geom_treemap_text(colour = "black",place = "centre",size=12)
treemap <- treemap + scale_fill_brewer(palette = 'Blues') + theme(legend.position = "none")
treemap <- treemap + labs(title = "Obama Approval Ratings") 
treemap


#reading in population data
pop_df <- read_excel("us-population-by-age.xls")
pop_df <- na.omit(pop_df)
colnames(pop_df)

#Renaming columns
pop_df <- pop_df %>%
  rename(Year = "...1")
pop_df

str(pop_df)

cbind(lapply(lapply(pop_df, is.na), sum))

#Convert year to number
pop_df <- transform(pop_df, Year = as.numeric(Year))

#Creating Area Chart
areachart <- ggplot(pop_df, aes(x=Year,y= X65.))
areachart <- areachart+geom_area( fill="#69b3a2", alpha=0.4)
areachart <- areachart+labs(title = "65+ US Population Growth")+ylab("Percent of Population")
areachart
                   





