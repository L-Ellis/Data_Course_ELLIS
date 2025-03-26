###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Leah Ellis
# Assignment 3 | BIOL 3100 001 
# Professor Liang

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

library(tidyverse)
iris[seq(2,nrow(iris),2), ]
 

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr <- apply(iris, 2, as.character)
iris_chr %>% str


# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area = iris$Sepal.Length * iris$Sepal.Width


# 4.  Add Sepal.Area to the iris data frame as a new column

iris <- iris %>% 
  mutate("Sepal.Area" = Sepal.Area) %>% 
  print


# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
      # (name it big_area_iris)

big_area_iris <- iris %>% 
  filter(Sepal.Area > 20.0) %>% 
  print


# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated
