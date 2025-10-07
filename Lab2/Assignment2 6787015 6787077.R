#Assignment2
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077
setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/Lab2")
getwd()

library(dplyr)
library(ggplot2)
ott <- read.csv("otter.csv")
head(ott)

#1) A boxplot (with raw data) comparing the body temperature ranges in the two 
#individuals. 

ggplot(ott, aes(x= animal, y=body.temp ,fill = animal))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.3), alpha = 0.05)+
  xlab("Animal")+
  ylab("Body temparature (celsius)")+
  theme_classic()

#2) A single graph of boxplots (with raw data) comparing body temperature between 
#these two otters while being active and inactive (so total of four box and whisker plots).

ggplot(ott, aes(x= animal, y=body.temp ,fill = active))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.width = 0.15), alpha = 0.05)+
  xlab("Animal")+
  ylab("Body temparature (celsius)")+
  theme_classic()

#3) A scatterplot of heart rate as a function of body temperature combining data from 
#both individuals. Distinguish whether animals were active or inactive and add a 
#regression line to each group of points
                     
ggplot(ott, aes(x=body.temp, y=heart.rate, colour = active))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = lm, fullrange = TRUE, aes(fill = active))+
  xlab("Body temparature (celsius)")+
  ylab("Heart rate (beats per minute)")+
  theme_classic()                
