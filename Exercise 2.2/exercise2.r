# Exercise 2.2
# DSC640
# Victoria Hall
# 4.7.2022

#Loading libarary
library(tidyverse)
library(scales)

#Reading in data
pop_df<-read.csv("world-population.csv")

pop_df

#Creating line chart
linechart <- ggplot(pop_df, aes(x=Year,y=Population))+geom_line(size=1, color = '#808080')

#Adding titles
linechart <- linechart+labs(title = "World Population", subtitle = '1960-2009')+
  theme(plot.title = element_text(size=20, face = "bold",hjust = 0.5),plot.subtitle = element_text(size=13, face = 'bold',hjust = 0.5))

#Editing y axis
linechart <- linechart + scale_y_continuous(labels = unit_format(unit = 'B', scale = 1e-9))

linechart <- linechart + annotate('text', x = 1970, y = 6500000000,label= "*Population has doubled in 50 years")

linechart <- linechart+theme(panel.background = element_blank())
linechart <- linechart+theme(axis.line = element_line(colour= "black"))
linechart


#Creating Step Chart
stepchart <- ggplot(pop_df, aes(x=Year,y=Population))+geom_step()
stepchart <- stepchart + labs(title = "World Population", subtitle = '1960-2009')
stepchart <- stepchart + theme(plot.title = element_text(size=20, face = "bold",hjust = 0.5),plot.subtitle = element_text(size=13, face = 'bold',hjust = 0.5))
stepchart <- stepchart + scale_y_continuous(labels = unit_format(unit = 'B', scale = 1e-9))
stepchart <- stepchart + theme(panel.background = element_blank())
stepchart <- stepchart + annotate('text', x = 1970, y = 6500000000,label= "*Population growth appears even over time")
stepchart <- stepchart +theme(axis.line = element_line(colour= "grey"))
stepchart
