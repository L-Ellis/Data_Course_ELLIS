###########################
#                         #
#       Assignment        #
#                         # 
###########################

# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

iris_even <- iris[seq(2,150,2),]
iris_even


# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr <- as.character(iris)
iris_chr

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area <- c(iris$Sepal.Length * iris$Sepal.Width)
Sepal.Area


# 4.  Add Sepal.Area to the iris data frame as a new column
# iris_copy <- iris
# iris_copy$Sepal.Area <- as.factor(Sepal.Area) # Tested it on a new dataframe because I was worried about breaking something for some reason

iris$Sepal.Area <- Sepal.Area
iris

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)

big_area_iris <- iris[iris$Sepal.Area > 20,]
big_area_iris


# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
# to canvas
# I should be able to run your R script and get all the right objects generated