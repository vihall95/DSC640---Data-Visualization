#Week 11&12 Exercise 6.2
#Victoria Hall
#6.1.2022
#DSC640


#loading libraries
library(tidyverse)




#Reading in data
crime_df<-read.csv("crimeratesbystate-formatted.csv",header = TRUE)
head(crime_df,5)

mean(crime_df$murder)

#Building Histogram
hist <- ggplot(crime_df,aes(murder))+geom_histogram(color="#000000",fill="#000080") #initial plot
hist <- hist+geom_vline(aes(xintercept = mean(murder)), color = "#000000", size = 1.25,linetype="dashed")#adding mean line
#Adding titles,labels
hist <- hist+labs(title="U.S. Murder Rate Distribution",subtitle = "DC Murder Rates are 5 times the national average",x="Murder Rate",y="Number of States")
hist <- hist+theme(plot.title.position = "plot") #repositioning titles
hist <- hist+theme_classic() #Removing gridlines
hist <- hist+annotate("segment", x = 35.4, xend = 35.4, y = 9, yend = 2 ,colour = "red", size=2, alpha=0.6, arrow=arrow()) #adding arrow
hist <- hist+ annotate("text", x = 35.4,y=10, label="Washington DC") #labeling DC pointing
hist <- hist+annotate("text",x=8,y=12,label="Average 5.3")
hist



#Box Plot

#reading in education data
education_df<-read.csv("education.csv",header=TRUE)
head(education_df,5)

#Creating a dataframe of just SAT scores
scores <- subset(education_df, select = c(reading,math,writing))
head(scores,5)

require(reshape2)
#plotting the columns. Had to reshape frame for plotting
box <- ggplot(data = melt(scores), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))
box <- box + stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "white") #Adding mean point
box <- box+labs(title="U.S. SAT Scores",subtitle = "Iowa has the highest scores",x='Subject',y='Score') #labels and titles
box <- box+theme_classic() #removing gridlines
box <- box + theme(plot.title.position = "plot") #realigning
box <- box + theme(legend.position="none")
box <- box +labs(caption="*White star is mean value")
box


#Bullet chart
library(plotly)

#Highest Score
education_df[which.max(education_df$math),]

#US 
education_df[1,]

#Lowest Score
education_df[which.min(education_df$math),]

mean(education_df$math)

quantile(education_df$math, prob=c(.25,.5,.75))

#Building Chart
fig <- plot_ly()
fig <- fig %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 451, #DC Math Score
    delta = list(reference = 538), #mean score
    domain = list(x = c(0.25, 1), y = c(0.08, 0.25)), #figure placement
    title =list(text = "DC Math SAT Score"),
    gauge = list(
      shape = "bullet",
      axis = list(range = c(200, 800)), #axis start at 200 because you can't score less than 200
      threshold = list(
        line= list(color = "grey", width = 2),
        thickness = 0.75,
        value = 525.5),#mean value
      steps = list(
        list(range = c(200, 505.75), color = "E48F72"), #25 percentile
        list(range = c(505.75, 571.25), color = "lightyellow"), #50 percentile
        list(range = c(571.1, 800), color = "lightgreen")), #75 percentile
      bar = list(color = "lightgrey")))
fig <- fig %>%
  add_trace(
    type = "indicator",
    mode = "number+gauge+delta",
    value = 515,
    delta = list(reference = 538),
    domain = list(x = c(0.25, 1), y = c(0.4, 0.6)),
    title = list(text = "US Math Score"),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(200, 800)),
      threshold = list(
        line = list(color = "grey", width= 2),
        thickness = 0.75,
        value = 525.5),
      steps = list(
        list(range = c(200, 505.75), color = "E48F72"),
        list(range = c(505.75, 571.25), color = "lightyellow"),
        list(range = c(571.1, 800), color = "lightgreen")),
      bar = list(color = "lightgrey"))) 
fig <- fig %>%
  add_trace(
    type =  "indicator",
    mode = "number+gauge+delta",
    value = 615,
    delta = list(reference = 538 ),
    domain = list(x = c(0.25, 1), y = c(0.7, 0.9)),
    title = list(text = "Iowa Math SAT Score",fontsize=12),
    gauge = list(
      shape = "bullet",
      axis = list(range = list(200, 800)),
      threshold = list(
        line = list(color = "grey", width = 2),
        thickness = 0.75,
        value = 525.5),
      steps = list(
        list(range = c(200, 505.75), color = "E48F72"),
        list(range = c(505.75, 571.25), color = "lightyellow"),
        list(range = c(571.1, 800), color = "lightgreen")),
      bar = list(color = "lightgrey")))

fig

#line chart
head(education_df,5)


#Building line plot
line <-ggplot(education_df,aes(x=percent_graduates_sat, y=math))
line <- line + geom_line(color = "000080", size=2,alpha=.8) #Adding line
line <- line + theme_classic() #classic theme wihtout grid lines
line <- line + labs(x="Percent Students Taking SAT","Math SAT Score",
                    y = "Math Score",
                    title = "Math SAT Scores vs Percent of Students that Took the exam",
                    caption = "Scores decrease with more students\ taking the test.Perhaps only high acheiving students in states with low participation are taking the exam")
line
