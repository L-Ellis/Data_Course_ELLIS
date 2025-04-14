library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# 1. load the “/Data/mushroom_growth.csv” data set
mushrooms <- read_csv("../../Data/mushroom_growth.csv")






# 2. create several plots exploring relationships between the response and predictors

# 3. define 4 models that explain the dependent variable “GrowthRate”


# calculates the mean sq. error of each model
# selects the best model you tried


# adds predictions based on new hypothetical values for the independent variables used in your model
# plots these predictions alongside the real data



## get mean squared error:
# mean(mod1$residuals^2)
