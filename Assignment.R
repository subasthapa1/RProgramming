#Importing libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
#Reading the csv file
melanoma <- read.csv("D:/Stastistic/Assignment/ShinyAppForAnalysis/Data/melanoma-2.csv")

#Printing few rows of our dataset
head(melanoma)

#Tibble
melanoma <- melanoma %>%
  mutate(status = recode_factor(status, `1` = "died", `2` = "alive", `3` = "diedfromotherdisease")) %>%
  mutate(sex = recode_factor(sex, `0` = "female", `1` = "male")) %>%
  mutate_at(c("ulcer"),
            ~ recode_factor(.x, `0` = "no", `1` = "yes"))

#Removing Null values
melanoma <- na.omit(melanoma)

#Filter
female_patients <- filter(melanoma, sex==0)
male_patients <- filter(melanoma, sex==1)

#Printing new dataset
head(female_patients)

#Arrange data
female_pt_byage <- melanoma %>% group_by(status) 
+ female_pt_byage %>% arrange(year)

asc_data <- arrange(melanoma, desc(year))

#Filter
deadalive_status <- filter(melanoma, status %in% c(1, 2))

#Thickness of tumor vs time the patient they lived
ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness, size=status))

#Groupby gender using pipe
group_dataset <- melanoma %>% 
  group_by(sex)
   


ggplot(data = tumsize, mapping = aes(x = thick, y = timelived))
+geom_smooth(se = FALSE)

#Categorical variables
ggplot(data = melanoma) +
  geom_bar(mapping = aes(x = status))

ggplot(data = melanoma) +
  geom_bar(mapping = aes(x = sex))

#Continuous variables
ggplot(data = melanoma) +
  geom_histogram(mapping = aes(x = age), binwidth = 0.9)

#freqpoly plot
ggplot(data = melanoma, mapping = aes(x = age, colour = status)) +
  geom_freqpoly(binwidth = 0.1)

#Histogram
ggplot(data = melanoma, mapping = aes(x = thickness)) +
  geom_histogram(binwidth = 2)

ggplot(melanoma) + 
  geom_histogram(mapping = aes(x = age), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

#Point plot
ggplot(data = melanoma, mapping = aes(x = thickness, y = time, color= age)) + 
  geom_point(na.rm = TRUE)

#Density
ggplot(data = melanoma, mapping = aes(x = age, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = status), binwidth = 2)

#Facet wrap
ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness)) + 
  facet_wrap(~ sex, nrow = 1)

ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness)) + 
  facet_wrap(~ ulcer, nrow = 1)

ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness)) + 
  facet_wrap(~ status, nrow = 1)

#Boxplot: Status of patient age
ggplot(data = melanoma, mapping = aes(x = status, y = age)) +
  geom_boxplot()

#Boxplot: Thickness and age
ggplot(data = melanoma, mapping = aes(x = status, y = thickness)) +
  geom_boxplot()

#Mean time male and female patients lived
ggplot(data = melanoma, mapping = aes(x = sex, y = time)) +
  geom_boxplot()

#Tumor thickness and ulcer
ggplot(data = melanoma, mapping = aes(x = ulcer, y = thickness)) +
  geom_boxplot()

#Mean size of thickness of tumor in male and female patients
ggplot(data = melanoma, mapping = aes(x = sex, y = thickness)) +
  geom_boxplot()

#Facet wrap
ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness)) + 
  facet_grid(sex ~ status)

ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = time, y = thickness)) + 
  facet_wrap(~ sex, nrow = 2)

#Smooth plot
ggplot(data = melanoma) + 
  geom_smooth(mapping = aes(x = time, y = thickness, size=status))

#point and smooth together
ggplot(data = melanoma) + 
  geom_point(mapping= aes(x = time, y = thickness)) +
  geom_smooth(mapping = aes(x = time, y = thickness)) + 
  facet_wrap(~ sex, nrow = 2)

ggplot(data = melanoma, mapping = aes(x = time, y = thickness, color = sex)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = melanoma, mapping = aes(x = age, y = thickness, color = sex)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

#Barchart
ggplot(data = melanoma) + 
  geom_bar(mapping = aes(x = sex))

ggplot(data = melanoma) + 
  geom_bar(mapping = aes(x = status))

#Statisticalsummary
ggplot(data = melanoma) + 
  stat_summary(
    mapping = aes(x=sex, y=time),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#Boxplot
ggplot(data = melanoma, mapping = aes(x = sex, y = time, group=sex)) + 
  geom_boxplot() +
  labs(x="Gender", y="Time", title="Time Lived by different gender" )

#boxplot
ggplot(data = melanoma, mapping = aes(x = ulcer, y = time, group=ulcer)) + 
  geom_boxplot()

#Scatterplot
ggplot(data = melanoma) + 
  geom_point(mapping = aes(x = thickness, y =time, color=sex), alpha = 0.4)

ggplot(data = melanoma) + 
  geom_point(mapping = aes(y = thickness, x =year, color=sex), alpha = 0.4)
