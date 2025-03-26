## Class_Notes_1-30-2025
library(tidyverse)

## Load mtcars
##  1. Find cars with an wt greater than 3 and cyl equal to 8,
## then save to a new object
my_mtcars <- mtcars[mtcars$wt > 3, ]
my_mtcars <- mtcars[mtcars$cyl == 8, ]
my_mtcars %>% view

## 2. Calculate the average mpg of the new object
mean(my_mtcars$mpg)

## 3. Create a new numeric row named "hp.cyl"
## Which is calculated by the dividing hp by cyl
my_mtcars$hp.cyl <- my_mtcars$hp / my_mtcars$cyl 
my_mtcars <- my_mtcars %>% as.data.frame()
my_mtcars %>% view

## 4. Save this as a .csv file on your laptop. and open it.
write.csv(hp.cyl, paste0(getwd(), '/Classes/Data/class_practice_1-30-2025.csv'))

## 5. All of this, but using tidyverse instead.
mtcars %>% 
  filter(wt > 3, cyl == 8) %>% 
  mutate("hp/cyl" = hp.cyl <- hp/cyl) %>% 
  write_csv(paste0(getwd(), '/Classes/Data/class_practice_1-30-2025.csv'))


## 6. Mess with Palmer penguins
library(palmerpenguins)

penguins %>% 
  filter(bill_length_mm > 40.0, sex == 'female') %>%
  view

## 7. Calculate average weight of female penguins with bill length greater than 40 mm.
dat_bill <- penguins %>% 
  filter(bill_length_mm > 40.0, sex == 'female') %>%
  pluck('body_mass_g') %>% 
  mean(na.rm = TRUE)

## 8. Calculate mean body mass of each species on each island using group_by and summarize.
penguins %>% 
  filter(bill_length_mm > 40.0, sex == 'female') %>%
  group_by(species, island) %>% 
  summarize('mean_body_mass_g' = mean(body_mass_g),
            'max_body_mass_g' = max(body_mass_g),
            'count' = n(),
            ) %>% 
  view %>%
  write.csv(paste0(getwd(), '/Classes/Data/Class_1-30-2025_penguins_2.csv'))




