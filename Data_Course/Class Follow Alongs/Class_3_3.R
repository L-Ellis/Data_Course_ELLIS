library(tidyverse)
library(modelr)
library(easystats)
library(palmerpenguins)
library(tidyr)

df <- penguins %>% 
  filter(!sex == "unsexed")

mod1 = glm(data=df, formula = flipper_length_mm ~ sex)
summary(mod1)

# males have a flipper length on average 7.5mm longer, 
# but that's just a prediction based on a limited dataset

aov_mod <- df %>% 
aov(data=., formula= flipper_length_mm ~ sex * species)

# + looks both sex and species on their own.
# * looks at both sex,s pecies and the interaction between the two. (still categorical) 

summary.aov(aov_mod)

# Only works for catagorical predictors
TukeyHSD(aov_mod) %>% plot()
# lwr and upr are minimum and max differences seen. 
# (Or are they the bounds of the confidence intervals? Both? Something like that)



# always plot before modelling! 
df %>% 
  ggplot(aes(x=factor(year), y=flipper_length_mm, fill =sex)) +
  facet_wrap(~species) +
  geom_boxplot()

# Does flipper length change from year to year?
aov_mod2 <- aov(data=df, formula= flipper_length_mm ~ factor(year) * species * sex)
TukeyHSD(aov_mod2)
TukeyHSD(aov_mod2) %>% plot()


# your tools:
# filter()
# aov()
# glm()


data("us_rent_income")
us <- us_rent_income


# predictors: state (NAME), income
tidy_rent <- us %>% pivot_wider(id_cols = NAME, names_from = variable, values_from = estimate)
# we just lost GEOID and moe, which is fine.


tidy_rent %>% 
  ggplot(aes(x=income, y=rent)) +
  geom_point() +
  geom_smooth(method="lm")


glm(data=tidy_rent, formula= income ~ rent) %>% summary()



# I should practice pivoting more!






