# Homework week 7

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(h2o)  

#Add ggplot2::mpg dataset.

data_my <- ggplot2::mpg 

# Make data ready for analysis doing pre-processing techniques.

str(data_my)
view(data_my)
nrows <- nrow(data_my)
ncomplete = sum (complete.cases(data_my))
ncomplete/nrows #answer is 1 it means we dont have any na value in our data
summary(data_my)
names(data_my)

# Splitting the data 

h2o_data <- data_my %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

h2o.init()

target <- "displ"
predictors <- data_my %>% select(-displ) %>% names()


model <- h2o.glm(
  x = predictors, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)


# Run GLM using following modelling structure. cty ~ year + cyl + displ. 

model2 <- h2o.glm(
  x = c("year","cyl","displ"), y = "cty",
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

# Print coefficients table. 

#Model 1

model@model$coefficients_table %>%
  as.data.frame() %>% select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

#Model2

model2@model$coefficients_table %>%
  as.data.frame() %>% select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# Give interpretation of results.

#Having p value to be less than 0.05 to be considered as statistically significant.

# Name your final homework Script as “Fuel_Economy_Data_Analysis”. 
# Create a new folder and add your Script to this folder, then add and make commits of your  changes. 
# Create repository named “Fuel_Economy_Data” in your Github account. 
# Push your homework Script to this repository. 
# Fork other users’ repositories, make changes as needed and make pull requests


#https://github.com/aliasgerovs/Fuel_Economy_Data_Analysis