#Lab1 Assignment
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077
setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/ITDS125_Lab_W4/w4")
getwd()

#R calculator
2+3
10-4
log(10)
log10(10)
sqrt(16)
sin(pi/2)
sin(45*pi/180)

#Create vectors
vec0 <- vector(mode = "numeric", length = 10) # Create a vector filled with 0
vec0

vec1 <- c(1:30, 35, 70) # Create a vector with specified values
vec1
length(vec1)
mean(vec1)
summary(vec1)
sd(vec1)

# Create and manipulate matrices
mat <- matrix(nrow = 3, ncol = 4) # Create an empty matrix
mat
mat[2, 4] <- 8 # Assign value to a specific element
mat
mat[1, ] <- c(1:4) # Assign values to the first row
mat[, 3] <- c(5:7) # Assign values to the third column
mat[2, 1:2] <- c(4, 5)  # Assign values to specific elements
mat[3, c(1:2, 4)] <- c(5, 6, 8) # Assign multiple values
mat

rownames(mat) <- c("student1", "student2", "student3")
mat
colnames(mat) <- c("test1", "test2", "test3", "test4")
mat

# Create and manipulate arrays
arr <- array(dim = c(3, 3, 4))  # Create a 3D array
arr[1, 1, 1] <- 8
arr[3, 3, 2] <- 16   # Assign value to a specific element
arr

# Import data
envi <- read.csv("environment.csv", header = TRUE) # Reads a CSV file into a data frame
head(envi, 10)
# Explore data
str(envi)   # View the structure of the data

mean(envi$rainfall.m) # Calculate the mean of a column
is.na(envi$rainfall.m) # Check for missing values
which(is.na(envi$rainfall.m)) # Find indices of missing values
mean(envi$rainfall.m, na.rm = TRUE) # Mean excluding missing values

envi$rainfall.m[is.na(envi$rainfall.m)] <- 0 # NA = 0
envi

# Work with character columns
is.character(envi$site)

envi$site <- as.character(envi$site)
envi$site[2:10] <- paste0("Site.", letters[2:10])
envi
envi$site[8] <- "Site.J"
envi

# Load dplyr
#install.packages("dplyr")
library(dplyr) # Load dplyr
glimpse(envi) # Explore and select data
select(envi, altitude.m) # Select specific columns
select(envi, altitude.m) # Exclude specific columns
slice(envi, 2) # Filter rows

# Add and arrange data
optimal_temp <- filter(envi, temperature.C > 18 | temperature.C > 20)
optimal_temp_log <- mutate(envi, logTemp = log(temperature.C))
arrange(optimal_temp_log, temperature.C)
arrange(optimal_temp_log, desc(logTemp))

# Without pipe
filtered_data <- filter(optimal_temp_log, altitude.m > 100)
filtered_data
arranged_data <- arrange(filtered_data, desc(altitude.m))
arranged_data

# With pipe
arranged_data_1 <- optimal_temp_log %>%
  filter(altitude.m > 100) %>%
  arrange(desc(altitude.m))
arranged_data_1