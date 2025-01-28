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
my_mtcars <- my_mtcars[my_mtcars$mpg > 20 && my_mtcars$cyl == 4,]
my_mtcars %>% view

