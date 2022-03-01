library(tidyverse)
library(modelr)
library(easystats)
library(GGally)

# plot cty mpg
df <- mpg

df %>% names()

ggpairs(df,columns = c("cty","displ","drv"))


df %>% 
  ggplot(aes(x=displ,y=cty,color=drv)) +
  geom_point()



# create models
mod1 <- glm(data = df,
            formula = cty ~ displ)
mod2 <- glm(data = df, 
            formula = cty ~ drv)
mod3 <- glm(data = df, 
            formula = cty ~ displ * drv)

summary(mod1)
summary(mod2)
summary(mod3)

compare_models(mod1,mod2,mod3,style="se_p")
compare_performance(mod1,mod2,mod3)


# model predictions (no model is perfect, these will not be perfect predictions)
hyp_preds <- data.frame(displ=6,drv="r") %>% 
  gather_predictions(mod1,mod2,mod3)

hyp_preds

