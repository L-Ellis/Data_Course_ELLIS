library(tidyverse)
library(modelr)
library(easystats)

df <- read.csv("./Data/GradSchool_Admissions.csv")
df %>% glimpse()

df <- df %>% 
  mutate(admit=case_when(admit == 1 ~ TRUE, TRUE ~ FALSE)) %>% 
  mutate(rank = factor(rank))

library(GGally)
df %>% ggpairs()

# Categorical / true false

df %>% 
  ggplot(aes(x=admit,y=gre)) +
  geom_bar(stat="identity")

# poisson distribution family looks interesting
# true-false or catagorical such and such tends to use the "binomial" dist family

mod1 <- glm(data=df, formula=admit ~ gre + gpa + rank, family = "binomial")
df <- add_predictions(df, mod1, type = "response") # the pred in this case is the percent chance of a student getting into grad school given their gpa, gre, and rank stats
# response means that we want our prediction values to be on the same scale as the responding variable?

df %>% 
  ggplot(aes(x=gre,y=pred,color=rank)) +
  geom_smooth(method = "loess")

# sidenote: amount of prior research experience is the best predictor of phd's.
# Oxford Yale Harvard MIT

install.packages("palmerpenguins")
library(palmerpenguins)

penguins <- penguins %>% 
  mutate(opinion = case_when(body_mass_g > 5000 ~ TRUE, TRUE ~ FALSE))

mod2 <- glm(data=penguins, formula=opinion ~ bill_length_mm + bill_depth_mm, family="binomial")

penguins <- add_predictions(penguins,mod2, type = "response")
  
# prediction of how likely the penguin is to be above a threshold weight using bill_length_mm

penguins %>% 
  ggplot(aes(x=bill_length_mm,y=pred)) +
  geom_smooth()




# Cross validation is splitting your data set into two parts randomly, 
# we use this data later to compare out model against generated from the first set

training_set <- penguins %>% # typically you want the bulk of the data put in the training set, rather than the testing set
  slice_sample(prop = 0.75) # Take at random, 75% of the data set

testing_set <- anti_join(penguins,training_set) # everything not in the training set

# Gaussian is for normal distributions, glm defaults to Gaussian
mod3 <- glm(data=training_set,formula=bill_length_mm ~ sex * species * body_mass_g)

# cross validation
add_predictions(testing_set,mod3) %>% 
  ggplot(aes(x=body_mass_g)) +
  geom_point(aes(y=bill_length_mm, color = sex)) + # testing set
  geom_point(aes(y=pred)) # model

# running this over and over is called bootstrapping, run all this inside a for loop
# a bunch of times to account for more shuffles of the set


# MASS::stepAIC shows off some machine learning
# ?caret::nearZeroVar
# ?train
