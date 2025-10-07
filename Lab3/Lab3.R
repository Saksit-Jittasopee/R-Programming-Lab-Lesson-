#Lab3
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077

setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/Lab3_material")

library(dplyr)
library(ggplot2)

lady <- read.csv("ladybirds_morph_colour.csv")
head(lady)
str(lady)

#Ensure number is Numeric
lady$number <- as.numeric(lady$number)
# Check the data
glimpse(lady)

totals <- lady %>% group_by(Habitat, morph_colour)%>% summarise(total.number = sum(number))

ggplot(totals, aes(x = Habitat, y = total.number, fill = morph_colour)) +
  geom_bar(stat = "identity",position = 'dodge')+
  scale_fill_manual(values = c(black = 'black', red = 'red'))

lady.mat <-  xtabs (number ~ Habitat + morph_colour, data = lady)
lady.mat

#Test chi-square
chisq.test(lady.mat)

#If you see any values less than 5, consider using Fisher Exact Test instead
fisher.test(lady.mat)
