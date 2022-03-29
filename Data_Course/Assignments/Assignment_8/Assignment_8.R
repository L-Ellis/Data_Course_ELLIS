# Packages
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# Load
data("mtcars")
glimpse(mtcars)













# Make a new Rproj and Rscript in your personal Assignment_8 directory and work from there.
# Write a script that:
#   loads the “/Data/mushroom_growth.csv” data set

df <- read.csv("../../Data/mushroom_growth.csv")

# creates several plots exploring relationships between the response and predictors




# defines at least 4 models that explain the dependent variable “GrowthRate”


# calculates the mean sq. error of each model


# selects the best model you tried


# adds predictions based on new hypothetical values for the independent variables used in your model


# plots these predictions alongside the real data




# Upload responses to the following as a numbered plaintext document to Canvas:
#   Are any of your predicted response values from your best model scientifically meaningless? Explain.
# In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.
# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)


