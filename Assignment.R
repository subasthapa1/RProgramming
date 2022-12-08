#Importing libraries
library(tidyverse)
library(ggplot2)
#Reading the csv file
melanoma <- read.csv("D:/Stastistic/Assignment/ShinyAppForAnalysis/Data/melanoma-2.csv")

#Printing few rows of our dataset
head(melanoma)

#Removing Null values
melanoma <- na.omit(melanoma)
#Thickness of tumor vs time the patient they lived
ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness))