#Lab2
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077
setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/Lab2")
getwd()

#import library
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

#import data
compensation <- read.csv("compensation.csv")
head(compensation)
glimpse(compensation)

#scatter plot
ggplot(compensation,aes(x=Root,y=Fruit))+
  geom_point()

#customize
#get rid of the grey background
ggplot(compensation,aes(x=Root,y=Fruit))+
  geom_point()+
  theme_bw()

#Increase size of th plot + plot alpha
ggplot(compensation,aes(x=Root,y=Fruit, colour = Grazing))+ #color Grazing
  geom_point(size = 5,alpha =0.2)+ 
  theme_bw()

#add line and alter x-axis and y-axis labels
ggplot(compensation,aes(x=Root,y=Fruit))+
  geom_point(size = 3)+
  xlab("Root Biomass")+
  ylab("Fruit Production")+
  theme_bw()

#adjust color and adjust the shape
ggplot(compensation,aes(x=Root,y=Fruit, colour = Grazing, shape = Grazing))+
  geom_point(size = 3)+
  scale_colour_manual(values = c("red","pink"))+
  xlab("Root Biomass")+
  ylab("Fruit Production")+
  theme_bw()

#box-and-whisker plot
ggplot(compensation,aes(x=Root,y=Fruit))+
  geom_boxplot()+
  xlab("Grazing") +
  ylab("Fruit Production") +
  theme_bw() 

#histogram
ggplot(compensation,aes(x=Fruit))+
  geom_histogram()+
  xlab("Root Biomass")+
  ylab("Fruit Production")+
  theme_bw()

#if we want to adjust bin
ggplot(compensation,aes(x=Fruit))+
  geom_histogram(bins = 10)+
  theme_bw()

#adjust binwidth
ggplot(compensation,aes(x=Fruit))+
  geom_histogram(binwidth = 15)+
  theme_bw()

#split the graph
ggplot(compensation,aes(x=Fruit))+
  geom_histogram(binwidth = 15)+
  facet_wrap(~Grazing)