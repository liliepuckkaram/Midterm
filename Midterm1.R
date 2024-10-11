library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#install.packages('psych')
library(psych)
#install.packages('gplots')
library(gplots)
library(tidyverse)

traindir <- "~/Downloads/SYS 3502/Data/TrainData"
sourcedir <-"~/Downloads/SYS 3502/Data/TrainData"

setwd(traindir)
getwd()

housing_prices <- read.csv("housing-prices.csv")
housing_prices

#3

y <- housing_prices$Price
ysort <- sort(y, decreasing = TRUE)
ysort

index_price <- which.max(housing_prices$Price)
index_price
sqft_expensive <- housing_prices$Size[index_price]
sqft_expensive

#2

index_low_price <- which.min(housing_prices$Price)
index_low_price

#4
# Plotting size
ggplot(data = housing_prices, aes(x =, y = Size)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Size") +
  labs(y = "Size")

#5
x <- housing_prices$Size
xsort <- sort(x, decreasing = TRUE)
xsort

#6 & 7 Outlier size: 4050 3990 3949 3153 3110
# = 18252/5 = 3650.4

#8

# Plotting Bath
ggplot(data = housing_prices, aes(x =, y = Baths)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Baths") +
  labs(y = "Baths")


#9

pairs.panels(housing_prices[,c("Baths", "Price", "Age", "Rooms", "Size")])
pairs.panels(housing_prices[,c("Rooms", "Size")])
#.57
pairs.panels(housing_prices[,c("Baths", "Size")])
#.67
pairs.panels(housing_prices[,c("Price", "Size")])
#.84
pairs.panels(housing_prices[,c("Age", "Size")])
#-.4


#10

ggplot(housing_prices, aes(x = Size)) +
  geom_histogram(bins = nclass.Sturges(housing_prices$Size), fill = NA, colour = "steelblue") + 
  ggtitle("Histogram of Size (Size) ") +
  labs(x = "Size", ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 45))

#12

filtered_data <- filter(housing_prices, Age =="Old")
filtered_data
old_prices <- sum(filtered_data$Price)
old_prices

#13

table(as.factor(housing_prices$Baths), as.factor(housing_prices$Rooms))
heatmap(table(as.factor(housing_prices$Baths), as.factor(housing_prices$Rooms)), Rowv = NA, Colv = NA, scale = "none")


# Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)


#15
avg_month_temp <- tapply(airquality$Temp, airquality$Month, mean, na.rm = TRUE)
avg_month_temp

#16

ggplot(airquality, aes(x = Solar.R)) +
  geom_histogram(bins = nclass.Sturges(airquality$Solar.R), fill = NA, colour = "steelblue") + 
  ggtitle("Histogram of Solar Radiation") +
  labs(x = "Radiation") +  
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 45)) 

#19

pairs.panels(airquality[,c("Wind", "Ozone")])
pairs.panels(airquality[,c("Temp", "Ozone")])
pairs.panels(airquality[,c("Solar.R", "Ozone")])
pairs.panels(airquality[,c("Wind", "Solar.R")])
pairs.panels(airquality[,c("Wind", "Temp")])
pairs.panels(airquality[,c("Wind", "Ozone","Solar.R", "Temp")])


#23

may_wind <- airquality %>% filter(Month == 5)
ggplot(data = may_wind, aes(x = Month, y = Wind)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of May Wind") +
  labs(y = "Wind")
#avg = 11.5
#IQR =5


june_wind <- airquality %>% filter(Month == 6)
ggplot(data = june_wind, aes(x = Month, y = Wind)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of June Wind") +
  labs(y = "Wind")
#9
#IQR = 4
july_wind <- airquality %>% filter(Month == 7)
ggplot(data = july_wind, aes(x = Month, y = Wind)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of July Wind") +
  labs(y = "Wind")
#IQR = 3
#med = 8.7

aug_wind <- airquality %>% filter(Month == 8)
#aug_iqr <- IQR(aug_wind, na.rm = TRUE)
ggplot(data = aug_wind, aes(x = Month, y = Wind)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of August Wind") +
  labs(y = "Wind")
#med: 8.5

sept_wind <- airquality %>% filter(Month == 9)
ggplot(data = sept_wind, aes(x = Month, y = Wind)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Sept Wind") +
  labs(y = "Wind")

aug_iqr <- IQR(aug_wind$Wind, na.rm = TRUE)
aug_iqr
july_iqr <- IQR(july_wind$Wind, na.rm = TRUE)
july_iqr
june_iqr <- IQR(june_wind$Wind, na.rm = TRUE)
june_iqr
may_iqr <- IQR(may_wind$Wind, na.rm = TRUE)
may_iqr
