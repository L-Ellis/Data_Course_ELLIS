
# Leah Ellis
# Exam 2
# 4/1/2025

# import libraries
library(tidyverse)
library(ggplot2)
library(modelr)

# 1.
  # load data
df <- read.csv("./unicef-u5mr.csv")

# 2. 
  # clean the data and assign it to df1. tidy format.
df1 <- df %>%  
  pivot_longer(cols=starts_with("U5MR."), names_prefix = "U5MR.", names_to = "Year", values_to = "U5MR") %>% # pivot longer  
  filter(!is.na(U5MR)) %>%  # remove NA values
  mutate(Year = parse_date(Year, format="%Y")) # convert Year to datetime format

# 3.
  # plot each countries U5MR over time and facet by continent 
df1 %>% 
  ggplot(aes(x=Year,y=U5MR,group=CountryName)) + theme_bw() +
  facet_wrap(vars(Continent)) + geom_line(size=0.5) 

# 4.
  # save to file
ggsave(file="./ELLIS_Plot_1.png", units="px", width=1920, height=1080) 

# 5.
  # plot mean U5MR and color by continent
df1 %>% 
  group_by(Continent, Year) %>% 
  summarise(Year=Year, Mean_U5MR=mean(U5MR), .groups='drop') %>%
  ggplot(aes(x=Year,y=Mean_U5MR,group=Continent,colour=Continent)) + theme_bw() + geom_line(size=2)

# 6.
  # save to file
ggsave(file="./ELLIS_Plot_2.png", units="px", width=1920, height=1080)

# 7. create three linear regression models of U5MR
ex <- df1 %>% resample_partition(c(test = 0.25, train = 0.75))
mod1 <- ex$train %>% lm(data=.,U5MR ~ Year)
mod2 <- ex$train %>% lm(data=.,U5MR ~ Year + Continent)
mod3 <- ex$train %>% lm(data=.,U5MR ~ Year + Continent + Year:Continent)

# 8.
  # compare the three models using rmse to see fit to the data. 
rmse(mod1,df1) # ~73.43
rmse(mod2,df1) # ~52.62
rmse(mod3,df1) # ~49.94
  # the lowest root-mean-squared-error is that of model 3, at around ~49.94.
  # so, model 3 has the best performance.

# 9.
  # plot the three models and save plot to file  
df2 <- df1 %>% gather_predictions(mod1,mod2,mod3)
df2 %>% 
  ggplot(aes(x=Year,y=pred,group=Continent,colour=Continent)) + theme_bw() + 
  facet_wrap(vars(model)) + geom_line(size = 1.0)
ggsave(file="./ELLIS_Plot_3.png", units="px", width=1920, height=1080)

# 10.
  # test model 3 against real data
ecuador_test = data.frame(CountryName="Ecuador",Continent="Americas",Region="South America",Year=parse_date("2020", format="%Y"))
ecuador_2020_real = 13
diff_1 <- ecuador_2020_real - predict.lm(mod3, ecuador_test) 
print(diff_1)
  # model 3's prediction is only about 23 off from the actual data.

  # create a better model for Ecuador
mod4 <- ex$train %>% lm(data=.,U5MR ~ Year + Region + CountryName + Year:Region)
diff_2 <- ecuador_2020_real - predict.lm(mod4, ecuador_test)
print(diff_2)
  # only ~9 off, not too bad!
