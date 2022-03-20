# Packages

library(tidyverse)
library(tidyr)
library(dplyr)
library(modelr)
library(easystats)
library(GGally)
library(purrr)
library(janitor)

# Set themes




# Functions
pivot_by_sex <- function(data, sex, sex_prefix) {
  pivot_longer(data=data, cols=starts_with(sex_prefix),names_to = "measurement",values_to = "value",names_prefix = sex_prefix) %>% 
    mutate(sex = sex)
} # Tidy by sex

# Import data
df <- read.csv("https://raw.githubusercontent.com/gzahn/Data_Course/master/Data/Bird_Measurements.csv")

df_tidy <- df %>% 
  janitor::clean_names() %>% 
  select(-ends_with("_N"))

df_male <- df %>% 
  select(-starts_with("f_"), -starts_with("unsexed_")) %>%
  pivot_by_sex(data=.,sex="Male",sex_prefix="m_")

df_female <- df %>% 
  select(-starts_with("m_"), -starts_with("unsexed_")) %>% 
  pivot_by_sex(data=.,sex="Female",sex_prefix="f_")

df_unsexed <- df %>% 
  select(-starts_with("m_"), -starts_with("f_")) %>% 
  pivot_by_sex(data=.,sex="Unsexed",sex_prefix="unsexed_")

  # Combine
df_tidy <- df_male %>% 
  full_join(df_female) %>% 
  full_join(df_unsexed)

df_tidy <- df_tidy %>% 
  pivot_wider()


df_tidy <- df_tidy %>% 
  pivot_wider(., # necessary fix for unknown error
              names_from = measurement, 
              values_from = value) # something's really wrong here


  # Visualize Data
df_tidy %>% 
  ggplot(aes(x=Egg_mass,y=F_mass, color=sex)) +
  facet_wrap(~sex) +
  geom_point()


  # test if mass is normally distributed
df_tidy %>% 
  ggplot(aes(x=M_mass)) +
  geom_density()
  # nope.

  # try to force it? 
df_tidy %>% 
  filter(M_mass <= 250) %>% 
  ggplot(aes(x=M_mass)) +
  geom_density()
  # how about we don't throw out tons of data

  # use a transformation
df_tidy %>% 
  ggplot(aes(x=log10(M_mass))) +
  geom_density()

  # Test how close to a normal dist
shapiro.test(df_tidy$M_mass)

shapiro.test(log10(df_tidy$M_mass))


# Model and test hypotheses
mod <- glm(data = df_tidy, formula = M_mass ~ Egg_mass)

  # start sink # "sync"
sink("./output/stat_table.txt") # sink("./output/stat_table.txt", append = TRUE)
summary(mod)
sink(NULL)
 # stop sink


  # for big output
library(broom)
tidy(mod) %>% 
  filter(p_value < 0.05)

library(easystats)
report(mod)





