#Lab1 Assignment
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077
setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/ITDS125_Lab_W4/w4")
getwd()

#1
library(dplyr)
comp <- read.csv("compensation.csv", header = TRUE) # Reads a CSV file into a data frame
#2
glimpse(comp) 
#3
comp$Root[is.na(comp$Root)] <- 0 # NA = 0
mean(comp$Root)
#4
lo_hi_fruit <- filter(comp, Fruit < 20 | Fruit > 80)
lo_hi_fruit 
arrange(lo_hi_fruit, Fruit) 
#5
compensation_trans <- mutate(lo_hi_fruit , sqrt_Fruit = sqrt(Fruit))
compensation_trans
slice(comp, 1:15)
#6
compensation_trans <- slice(comp, 1:15) %>%
  arrange(desc(Fruit))
compensation_trans
#7 Without Pipe
filtered_data <- filter(comp , Fruit > 50)
arranged_data_1 <- arrange(filtered_data, Fruit)
arranged_data_1
#7 With Pipe
arranged_data_2 <- comp %>%
  filter(Fruit > 50) %>%
  arrange(Fruit)
arranged_data_2
