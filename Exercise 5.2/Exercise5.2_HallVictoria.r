#Week 9&10 Exercise 5.2
#Victoria Hall
#5.22.2022
#DSC640



#loading libraries
library(tidyverse)


setwd("C:/Users/hallt/Documents/DSC640 - Data Visualization/Exercise 5.2")

#reading in approval rating data
bball_df<-read.csv("ppg2008.csv",header = TRUE)
bball_df

#heat map

#adjusting row names
row.names(bball_df) <- bball_df$Name
bball <- bball_df[,2:20]

#Creating matrix instead of dataframe object
bball_matrix <- data.matrix(bball)

#Creating heatmap. Scaling the features and using a heat color mapping
bball_heatmap<-heatmap(bball_matrix, Rowv=NA,Colv = NA,col = heat.colors(256),scale='column',margins=c(5,10),main= "2008 NBA Per Game Metrics")


#Contour Chart
library(plotly)

fig <- plot_ly(
  type = 'contour',
  z = matrix(c(10, 10.625, 12.5, 15.625, 20, 5.625, 6.25, 8.125, 
               11.25, 15.625, 2.5, 3.125, 5, 8.125, 12.5, 0.625, 
               1.25, 3.125, 6.25, 10.625, 0, 0.625, 2.5, 5.625, 
               10), nrow=5, ncol=5),
  colorscale = 'Jet',
  autocontour = F,
  contours = list(
    start = 0,
    end = 8,
    size = 2,
    title="Example Contour Chart"
  )
)

fig



#Spatial Chart

## loading library
library(maps)

## reading in costco data
costcos <- read.csv("costcos-geocoded.csv",sep=",")

##Creating first layer by loading in map of US in grey
map(database='state',col='#cccccc')


##laying costco data over MAP
symbols(costcos$Longitude,costcos$Latitude,bg="#a3ffb4", fg="#ffffff",lwd=.5,circles=rep(1,length(costcos$Longitude)),inches = .05,add=TRUE)


