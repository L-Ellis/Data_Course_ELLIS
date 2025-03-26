library(tidyverse)

## 1. What type of object is  this?
str(mtcars)
data(mtcars)
dim(mtcars)
  # 32 by 11 data.frame made up of mostly num's

## 2. Find cars with an mpg greater than 20 an 4 cyl,
## then save them to a new object
my_mtcars <- mtcars[mtcars$mpg > 20, ]
my_mtcars <- mtcars[mtcars$cyl == 4, ]
my_mtcars %>% view


## Option 2: 1 Step
my_mtcars <- mtcars
my_mtcars <- my_mtcars[my_mtcars$mpg > 20 & my_mtcars$cyl == 4,]
my_mtcars %>% view

## 3. Convert mpg to a character data type.
my_mtcars <- mtcars
my_mtcars$mpg <- as.character(my_mtcars$mpg)
my_mtcars %>% str

## 4. Convert the entire data frame to character data type.

  # Option 1 : do it one at a time. (don't do this one)
my_mtcars <- mtcars
my_mtcars$mpg <- as.character(my_mtcars$mpg)
my_mtcars$cyl <- as.character(my_mtcars$cyl)
my_mtcars$carb <- as.character(my_mtcars$carb)
## ...
my_mtcars %>% str

  # Option 2 : Using a for loop
my_mtcars <- mtcars
for (col in names(my_mtcars)) {
  my_mtcars[, col] <- as.character(my_mtcars[, col])
}
my_mtcars %>% str

  # Option 3 : Using apply
my_mtcars <- mtcars
my_mtcars <- apply(my_mtcars, 2, as.character)
my_mtcars %>% as.data.frame %>% str

## 4. Write into a csv file
write.csv(mtcars, paste0(getwd(),'/Classes/Data/class_practice_1-28-2025.csv'), row.names = F)
