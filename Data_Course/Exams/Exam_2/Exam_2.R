library(tidyverse)
library(modelr)
library(janitor)
library(ggplot2)
library(GGally)
library(easystats)
library(performance)


###############
#    Tidy     #                                               
###############

df = read.csv("unicef-u5mr.csv")
df_tidy <- df %>% 
  pivot_longer(data=., cols=starts_with("U5MR."), 
               names_to = "Year", names_prefix = "U5MR.", names_transform = list(Year = as.numeric),
               values_to = "U5MR") %>% 
  drop_na() # Remove NA's
  # filter(!is.na(U5MR))


###############
#   Plot 1    #                                               
###############

df_tidy %>% 
  ggplot(aes(x=Year,y=U5MR, foo = CountryName)) +
  facet_wrap(~Continent) +
  geom_line() + 
  theme_bw()
ggsave(filename = 'ELLIS_Plot_1.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in') 
# I finally figured out why all my plots looked all pixelated! It's a windows specific thing, it just doesn't anti-alias by default.
# I guess I only need to worry about during exports, and I can fix it by just using type = "cairo"
# https://stackoverflow.com/questions/6023179/anti-aliasing-in-r-graphics-under-windows-as-per-mac 


###############
#   Plot 2    #                                               
###############

df_mean <- df_tidy  %>% 
  group_by(Year, CountryName) %>% 
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE), Year, Continent) %>% 
  group_by(Year, Continent) %>% 
  summarise(Mean_U5MR = mean(Mean_U5MR, na.rm = TRUE), Year) %>% 
  .[!duplicated(.),] # Only rows that are not duplicated

df_mean %>% 
  ggplot(aes(x=Year,y=Mean_U5MR, color=Continent)) +
  geom_line(size = 1.75) +
  theme_bw()
ggsave(filename = 'ELLIS_Plot_2.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in') 


###############
#   Models    #                                               
###############

mod1 <- df_tidy %>% glm(data=., formula= U5MR ~ Year)
mod2 <- df_tidy %>% glm(data=., formula= U5MR ~ Year + Continent)
mod3 <- df_tidy %>% glm(data=., formula= U5MR ~ Year * Continent)

compare_performance(mod1,mod2,mod3) %>% plot()
#  _________________________________________________________________________________________________________________                                                                                     
# | Model 3 seems to be the most accurate, though I admit I have no idea how to interpret the model indices chart.  |
# | I'm just going off of the fact that it compares the most unique variables with one another.                     |
# |_________________________________________________________________________________________________________________|                                                                                

# Add Predictions and tidy to make it easier to feed into ggplot
  # I really tried to use a deparse(substitute()) to make a naming scheme that could just look at the variable name of the list,
  # as it turns out, it really acts weird on lists/inside a function. # https://stackoverflow.com/questions/47904321/deparsesubstitute-returns-function-name-normally-but-function-code-when-cal
  # So I gave up on that mess so now all I have to do is be careful with the naming
add_model_predictions = function(data, ...) { 
  models = list(...) 
  # Loop through and add predictions
  i <- 0
  for(model in models) {
    data <- add_predictions(data=data,model=model,var=paste(sep="", "mod", toString(i+1)) )
    i <- i+1
  }
  # Pivot columns
  data <- pivot_longer(
    data=data, cols = starts_with(vars=colnames(data), match="mod"), names_to = "model", values_to = "pred")
  return(data)
}

df_pred <- add_model_predictions(df_tidy, mod1,mod2,mod3)
df_pred %>% 
  ggplot(aes(x=Year, y=pred, color=Continent)) +
  facet_wrap(~model) +
  geom_line(size = 1.33) +
  theme_bw() +
  xlab("Predicted U5MR")
ggsave(filename = 'ELLIS_Plot_3.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in') 


###############
#   Bonus     #                                               
###############

# Real U5MR was 12.827
CountryName <- "Ecuador"
Continent <- "Americas"
Region <- "South America"
Year <- 2020
det <- data.frame(Year,Continent,CountryName,Region)

# Get diff from target prediction. Just used to make labels
det_prediction_diff <- function(model, det, target = 12.953) {
  return(target - predict(object=model,det)) 
}

# Thrown together function used to stick that diff on the end of the facet label
diff_labeller = function(string) { 
  i <- 0
  for(s in string) {
    var <- get(s) # "mod1"
    diff <- det_prediction_diff(var,det)
    s <- paste(sep = " | off by ", s, toString( round(diff,2) ))
    
    string[i+1] <- s
    i <- i+1
  }
  return(string)
}

# Remove outliers
df_tidy <- df_tidy %>% drop_na() # Already did this previously, might as well do it again in case I change something
df_focused <- df_tidy %>% filter( !check_outliers(x=.,method="iqr")) # filter out outliers

# Try best old model without outliers
mod4 <- df_focused %>% glm(data=.,formula = U5MR ~ Year * Continent) # Ever so slightly better?

# New models
  # Since the main problem is dipping into the negatives, lines that are allowed curve about the place should help
mod5 <- df_focused %>% lm(data=.,formula = U5MR ~ poly(Year, 4)) # looks like it works well? Probably a coincidence.
mod6 <- df_focused %>% lm(data=.,formula = U5MR ~ poly(Year, 2) * Continent) # Winner!!

df_pred <- df_focused %>% add_model_predictions(mod1,mod2,mod3,mod4,mod5,mod6)
df_pred %>% 
  ggplot(aes(x=Year, y=pred, color=Continent)) +
  facet_wrap(~model, labeller = labeller(model = diff_labeller)) +
  geom_line(size = 1.33) +
  theme_bw() +
  xlab("Predicted U5MR")
ggsave(filename = 'ELLIS_Plot_4.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in') 

# Only 0.98 off! I'm rather proud of that, and that graph looks -tight-.
# But, I probably ought to compare against earlier values too, 
# it might be coincidentally hitting the target at that singular time in 2020.

# Benchmarking / Cross-Validation
diffs_mod1 <- 0; diffs_mod2 <- 0; diffs_mod3 <- 0;
for(i in 0:16) {
  training_set <- slice_sample(df_tidy,prop=0.75)
  testing_set <- anti_join(df_tidy,training_set)
  mod1 <- training_set %>% glm(data=.,formula = U5MR ~ Year * Continent)
  mod2 <- training_set %>% lm(data=.,formula = U5MR ~ poly(Year, 4)) 
  mod3 <- training_set %>% lm(data=.,formula = U5MR ~ poly(Year, 2) * Continent)

  testing_set <- testing_set %>% 
    group_by(Year, CountryName) %>% 
    summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE), Year, Continent, Region, CountryName, U5MR) %>% 
    group_by(Year, Continent) %>% 
    summarise(Mean_U5MR = mean(Mean_U5MR, na.rm = TRUE), Year, Continent, Region, CountryName, U5MR) %>% 
    .[!duplicated(.),]

  diffs_mod1[i+1] <- det_prediction_diff(mod1, det)
  diffs_mod2[i+1] <- det_prediction_diff(mod2, det)
  diffs_mod3[i+1] <- det_prediction_diff(mod3, det)
}
# Mean diffs
mean_diff_mod1 <- mean(diffs_mod1)
mean_diff_mod2 <- mean(diffs_mod2)
mean_diff_mod3 <- mean(diffs_mod3)
mean_diffs <- c(mean_diff_mod1, mean_diff_mod2, mean_diff_mod3)

# Another label making function
mean_diff_labeller = function(string) { 
  i <- 0
  for(s in string) {
    diff <- mean_diffs[i+1]
    s <- paste(sep = " | mean off by ", s, toString( round(diff,2) ))
    string[i+1] <- s 
    i <- i+1
  }
  return(string)
}

# # Expand prediction line out to 2020
#   # Too much work for the time being, the graph really doesn't like missing values
# ts_copy <- df_tidy %>% 
#   filter(Year == 2015) %>% 
#   mutate(U5MR = NA)
# for (i in 2016:2020) {
#   ts_copy <- mutate(ts_copy, Year = i)
#   testing_set <- full_join(testing_set, ts_copy)
# }

# Graph
testing_set <- testing_set %>% add_model_predictions(mod1,mod2,mod3) # Add predictions made with training set
testing_set %>% 
  ggplot(aes(x=Year,color=Continent)) +
  geom_line(aes(y=Mean_U5MR), alpha = 0.33) +
  geom_line(aes(y=pred), size = 1.33) +
  facet_wrap(~model, labeller = labeller(model = mean_diff_labeller)) +
  theme_bw() +
  xlab("Year") +
  ylab("Predicted U5MR and Mean_U5MR") +
  ggtitle("(Year * Continent) vs (poly(Year, 4)) vs (poly(Year, 2) * Continent)",
          subtitle = "With Mean_U5MR displayed in the background")
# ggsave(filename = 'ELLIS_Plot_5_1024i.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in') 
ggsave(filename = 'ELLIS_Plot_5_16i.png', dpi = 300, type = "cairo", width = 8, height = 4, units = 'in')

#  _______________________________________________________________________________________________________________________________________________                                                                                       
# | After doing some benchmarking(1024x, toned it down in the submit), the best model is off from the target by an average of -1.25 or so.       |
# | Judging from the graph, everything lines up well in the past as well.                                                                        |
# |______________________________________________________________________________________________________________________________________________|                                                                                













