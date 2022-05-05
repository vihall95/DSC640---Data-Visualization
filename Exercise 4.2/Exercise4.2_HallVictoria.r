# Week 7 and 8  Exercise
# Victoria Hall
# DSC640
# 5.5.2022


#loading libraries
library(tidyverse)


#reading in data
crime_df <- read.csv("crimerates-by-state-2005.csv")

#Finding max value
crime_df[which.max(crime_df$aggravated_assault),]


#Scatter Plot
scatter <- ggplot(crime_df, aes(x=robbery,y=aggravated_assault))+geom_point()
scatter <- scatter + labs(title = "2005 US Crime Rates",caption="As robbery increases, so does incidents of aggravated assault")
scatter <- scatter + annotate("text", x = 672.1,y=715, label="Washington DC")
scatter <- scatter + ylab("Aggravated Assault")+xlab("Robbery")
scatter

#Bubble Chart
bubble <- ggplot(crime_df,aes(x=robbery,y=aggravated_assault,size=population))+geom_point(alpha=0.5,shape=21,color="blue")
bubble <- bubble + xlim(0,800)+ylim(0,800)
bubble <- bubble + scale_size(range = c(.1,25), name='Population')+theme(legend.position = "none")
bubble <- bubble + annotate("text", x = 672.1,y=715, label="Washington DC")
bubble <- bubble +  ylab("Aggravated Assault")+xlab("Robbery")
bubble <- bubble +  labs(title = "2005 US Crime Rates",subtitle="Robbery Rates vs. Aggravated Assault",caption="Bubble size indicates state population size")
bubble



#Dropping DC because it is a large outlier and I want to focus on the majority distribution
crime_df1 <- crime_df[!(crime_df$state=='District of Columbia'),]

#density plot
densityplt <- ggplot(crime_df1,aes(x=robbery))+geom_density(color="darkblue",fill = "lightblue")
densityplt <- densityplt + geom_vline(aes(xintercept=mean(robbery)),color = "blue",linetype="dashed",size=1)
densityplt <- densityplt +labs(title = "2005 US Robbery Rates by state",subtitle = "Doesn't include DC",caption="Line = Mean")
densityplt
