###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Instructions ####
# Fill in this script with stuff that we do in class.
# It might be a good idea to include comments/notes as well so you remember things we talk about
# At the end of this script are some comments with blank space after them
# They are plain-text instructions for what you need to accomplish.
# Your task is to write the code that accomplished those tasks.

# Then, make sure to upload this to both Canvas and your GitHub repository



###########################
#                         #
#    Class 1/25/2022      #
#                         # 
###########################

iris 
class(iris)
iris$Species
unique(iris$Species)
table(iris$Species)

install.packages("easystats", repos = "https://easystats.r-universe.dev")
library("easystats")
library("tidyverse")

## dplyr::filter()
## stats::filter()

iris %>% class()
iris$Species %>% table()

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length,color=Petal.Length - Petal.Width)) + 
  geom_point() +
  geom_smooth()

iris$Sepal.Length %>% 
  mean()
iris$Sepal.Length > 6

setosa <- iris[iris$Species == "setosa",] # Get all from array at indices that match the expression
setosa$Sepal.Length %>% mean()

versicolor <- iris[iris$Species == "versicolor",] # Get all from array at indices that match the expression
versicolor$Sepal.Length %>% mean()

verginica <- iris[iris$Species == "verginica",] # Get all from array at indices that match the expression
verginica$Sepal.Length %>% sd()

iris %>% 
  group_by(Species) %>% 
  summarize(MeanSepLength = mean(Sepal.Length), # summarize() Is almost always used after group_by()
            SDSepLength = sd(Sepal.Length),
            MinSepLength = min(Sepal.Length))

iris$Sepal.Length %>% summary() # Overall look at a variable, it returns (Min, 1st Qu, Median, Mean, 3rd Qu, Max)

iris %>% 
  ggplot(aes(x=Sepal.Length,color = Species)) +
  geom_density()

library(palmerpenguins) # Data set with penguins
names(penguins) # The name of each variable (column)
summary(penguins)
table(penguins$species,penguins$island)

Adelie <- penguins[penguins$species == "Adelie",]
summary(Adelie)



###########################
#                         #
#         Info            #
#                         # 
###########################

# Vector operations! ####

# Vectors are 1-dimensional series of values in some order
1:10 # ':' only works for integers
letters # built-in pre-made vector of a - z



vector1 <- c(1,2,3,4,5,6,7,8,9,10)
vector2 <- c(5,6,7,8,4,3,2,1,3,10)
vector3 <- letters # letters and LETTERS are built-in vectors

vector1 + 5
vector2 / 2
vector1*vector2

vector3 + 1 # can't add 1 to "a"


# Logical expressions (pay attention to these...they are used ALL THE TIME)
vector1 > 3
vector1 >= 3
vector1 < 5
vector1 <= 5
vector1 == 7
letters == "a"
letters != "c"
letters %in% c("a","b","c","z")
vector1 %in% 1:6


# Data Frames ####
# R has quite a few built-in data sets
data("iris") # load it like this

# For built-in data, there's often a 'help file'
?iris

# "Iris" is a 'data frame.' 
# Data frames are 2-dimensional (think Excel spreadsheet)
# Rows and columns
# Each row or column is a vector


dat <- iris # can rename the object to be easier to type if you want

# ways to get a peek at our data set
names(dat)
dim(dat)
head(dat)


# You can access specific columns of a "data frame" by name using '$'
dat$Species
dat$Sepal.Length

# You can also use square brackets to get specific 1-D or 2-D subsets of a data frame (rows and/or columns)
dat[1,1] # [Rows, Columns]
dat[1:3,5]

vector2[1]
letters[1:7]
letters[c(1,3,5,7)]


# Plotting ####

# Can make a quick plot....just give vectors for x and y axes
plot(x=dat$Petal.Length, y=dat$Sepal.Length)
plot(x=dat$Species, y=dat$Sepal.Length)


# Object "Classes" ####

#check the classes of these vectors
class(dat$Petal.Length)
class(dat$Species)

# plot() function behaves differently depending on classes of objects given to it!

# Check all classes (for each column in dat)
str(dat)

# "Classes" of vectors can be changed if needed (you'll need to, for sure, at some point!)

# Let's try
nums <- c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console
nums_factor <- as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it


# convert numeric to character
as.character(vector1)
as.character(vector1) + 5

# convert character to numeric
as.numeric(vector3)




#check it out
plot(nums) 
plot(nums_factor)
# take note of how numeric vectors and factors behave differently in plot()




# Simple numeric functions
# R is a language built for data analysis and statistics so there is a lot of functionality built-in

max(vector1)
min(vector1)
median(vector1)
mean(vector1)
range(vector1)
summary(vector1)

# cumulative functions
cumsum(vector1)
cumprod(vector1)
cummin(vector1)
cummax(vector1)

# even has built-in statistical distributions (we will see more of these later)
dbinom(50,100,.5) # probability of getting exactly 50 heads out of 100 coin flips




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

library(tidyverse)

iris_chr <- iris[,] %>% 
  lapply(FUN=as.character) %>% 
  as.data.frame()

typeof
iris_chr[,1]

nrow(iris)
ncol(iris)
iris[1,0:5]


# iris_chr <- head(iris[c(names(iris))]) %>% 
  # lapply(FUN=as.character)



#cols <- as.character(iris)
#iris_chr[,cols] <- apply(iris_chr[,cols], 2, function(x) as.character(x))

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

