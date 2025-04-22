# install.packages("patchwork")
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)
library(patchwork)

# 1. load the “/Data/mushroom_growth.csv” data set
mushrooms <- read.csv("../../Data/mushroom_growth.csv")

# 2. create several plots exploring relationships between the response and predictors
  # arranged in the order of what I hypothesize mushrooms to be most effected by to least  
mushrooms %>% 
  mutate(Humidity = factor(Humidity, levels=c("Low", "High"))) %>%
  ggplot(aes(x=Humidity,y=GrowthRate,fill=Species)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal()
ggsave(file="./ELLIS_Response_and_Predictors_1.png", units="px", width=1920, height=1080) 

mushrooms %>% 
  mutate(across(Temperature, as.factor)) %>% 
  ggplot(aes(x=Temperature,y=GrowthRate,fill=Species)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal()
ggsave(file="./ELLIS_Response_and_Predictors_2.png", units="px", width=1920, height=1080) 

mushrooms %>% 
  mutate(across(Nitrogen, as.factor)) %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate,fill=Species)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal()
ggsave(file="./ELLIS_Response_and_Predictors_3.png", units="px", width=1920, height=1080) 

mushrooms %>% 
  mutate(across(Light, as.factor)) %>% 
  ggplot(aes(x=Light,y=GrowthRate,fill=Species)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal()
ggsave(file="./ELLIS_Response_and_Predictors_4.png", units="px", width=1920, height=1080) 


# 3. define 4 models that explain the dependent variable “GrowthRate”
mod1 <- mushrooms %>% glm(data=.,GrowthRate ~ Humidity)
mod2 <- mushrooms %>% glm(data=.,GrowthRate ~ Species + Humidity)
mod3 <- mushrooms %>% glm(data=.,GrowthRate ~ Species * Humidity * Temperature)
mod4 <- mushrooms %>% glm(data=.,GrowthRate ~ Species * Humidity * Temperature * Nitrogen * Light)

# 4. calculate the mean sq. error of each model
mod1_mean_sq <- mean(mod1$residuals^2)
mod2_mean_sq <- mean(mod2$residuals^2)
mod3_mean_sq <- mean(mod3$residuals^2)
mod4_mean_sq <- mean(mod4$residuals^2)

# 5. select the best model you tried
  # that appears to be model 4, as it has the lowest mean_sq value.
chosen_model <- mod4

# 6. adds predictions based on new hypothetical values for the independent variables used in your model



a <- data.frame(
  Species = sample(x = c('P.ostreotus','P.cornucopiae'), size=1),
  Light = sample(x = c(0, 10, 20, 30), size=1),
  Nitrogen = sample(x = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), size=1),
  Humidity = sample(x = c("Low", "High"), size=1),
  Temperature = sample(x = c(0, 5), size=1)
)


generate_data <- function() {
  a <- data.frame(
      Species = sample(x = c('P.ostreotus','P.cornucopiae'), size=1),
      Light = sample(x = c(0, 10, 20, 30), size=1),
      Nitrogen = sample(x = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), size=1),
      Humidity = sample(x = c("Low", "High"), size=1),
      Temperature = sample(x = c(0, 5), size=1)
  )
  return(a)
} 

test_data <- data.frame()
for (i in 1:100) {
  test_data <- rbind(test_data, generate_data())
}

test_data_with_predictions <- test_data %>% 
  mutate(GrowthRate = predict.glm(chosen_model,.), Data_Type = "Prediction")

# 7. plots these predictions alongside the real data

test_data_with_predictions_joined <- mushrooms %>% 
  mutate(Data_Type = "Real") %>% 
  rbind(., test_data_with_predictions) %>% 
  mutate(Data_Type = factor(Data_Type, levels=c("Real", "Prediction")))
 
plot1 <- test_data_with_predictions_joined %>% 
  mutate(Humidity = factor(Humidity, levels=c("Low", "High"))) %>%
  ggplot(aes(x=Humidity,y=GrowthRate,fill=Data_Type)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1.0,na.rm=TRUE) +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

plot2 <- test_data_with_predictions_joined %>% 
  mutate(across(Temperature, as.factor)) %>% 
  ggplot(aes(x=Temperature,y=GrowthRate,fill=Data_Type)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

plot3 <- test_data_with_predictions_joined %>% 
  mutate(across(Nitrogen, as.factor)) %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate,fill=Data_Type)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

plot4 <- test_data_with_predictions_joined %>% 
  mutate(across(Light, as.factor)) %>% 
  ggplot(aes(x=Light,y=GrowthRate,fill=Data_Type)) + 
  facet_wrap(vars(Species)) +
  geom_boxplot(alpha=1,na.rm=TRUE) +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

plot1 + plot2 + plot3 + plot4
ggsave(file="./ELLIS_Combined_Predictions.png", units="px", width=1920, height=1080)


# Are any of your predicted response values from your best model scientifically meaningless? Explain.

#   When ranges go beyond already observed values, the model tends to predict impossibly high growth rates. 

# In your plots, did you find any non-linear relationships? 

#   It appears as if all plots contain non-linear relationships, judging by how wildly inaccurate the linear models fit for them. However, I am keenly aware of the possibility that I totally messed something up here... it kind of looks like it.


# Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.

#   https://www.geeksforgeeks.org/non-linear-regression-in-r/
#   https://tuos-bio-data-skills.github.io/intro-stats-book/non-linear-regression-in-R.html   


# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)

#   # I do not know near enough math to find the appropriate values for this function, however it would look something like this:
#   nl <- read.csv("../../Data/non_linear_relationship.csv")
#   nl %>% ggplot(aes(x=predictor,y=response)) + geom_point()
#   nl_mod1 <- nl %>% nls(data=., predictor ~ SSlogis(response, 4.4, 2.2, 0.266))
