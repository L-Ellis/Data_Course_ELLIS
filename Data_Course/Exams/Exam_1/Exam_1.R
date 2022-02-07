###########################
#                         #
#            I            #
#                         # 
###########################

path <- "cleaned_covid_data.csv";
covid_df <- read.csv(path, stringsAsFactors = FALSE);










###########################
#                         #
#           II            #
#                         # 
###########################

library(tidyverse)
##?subset
covid_df.A <- subset(covid_df, grepl("^(a|A)", covid_df$Province_State))
covid_df.A$Province_State










###########################
#                         #
#          III            #
#                         # 
###########################

library(ggplot2)

covid_df.A %>% 
  ggplot(aes(x = as.Date(Last_Update), y = Deaths)) +
  facet_wrap(~covid_df.A$Province_State, scales = "free") +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  xlab(label = "Time")


## order(as.Date(covid_df.A$Last_Update))
## sp <- distinct(as.data.frame(covid_df.A$Province_State))
## spl <- nrow(sp)
## sp <- make.names(sp)
## state_count = count(as.data.frame(covid_df.A$Province_State))
## covid_df.A$Province_State.Levels <- sp;
## str(covid_df.A)










###########################
#                         #
#           IV            #
#                         # 
###########################

## lapply(covid_df$States, max(lapply(covid_df$Deaths, max)))
max_fatality_rate <- covid_df %>% 
  group_by(Province_State) %>% 
  slice(which.max(Deaths)) 
  ## filter(Deaths == max(Deaths, na.rm = TRUE))
## I'm a little worried that just taking the max per a given time period doesn't actually give a fatality rate for some reason, 
## it is compared against time though, so I won't mess with things further.
state_max_fatality_rate <- c("Province_State" = data.frame(), "Maximum_Fatality_Ratio" = data.frame())
state_max_fatality_rate$Province_State <- max_fatality_rate$Province_State; 
state_max_fatality_rate$Maximum_Fatality_Ratio <- max_fatality_rate$Deaths;
state_max_fatality_rate <- state_max_fatality_rate %>% 
  as.data.frame() %>% 
  arrange(desc(Maximum_Fatality_Ratio)) # I find it a little odd that desc() is applied *before* arrange(), you'd think it would just reverse things after the fact. 










###########################
#                         #
#           V             #
#                         # 
###########################

##length(state_max_fatality_rate$Province_State)
##length(state_max_fatality_rate$Maximum_Fatality_Ratio)
## # The length is the same? Why does the error say it isn't???
##state_max_fatality_rate$Province_State <- state_max_fatality_rate$Province_State %>% 
##as.factor() %>% 
##factor(levels = reorder(x = state_max_fatality_rate$Province_State, X = state_max_fatality_rate)) %>% 
##reorder(X = state_max_fatality_rate) %>% 
##levels() %>% 
##print()
##length(as.factor(state_max_fatality_rate$Province_State)) 
##length(as.factor(state_max_fatality_rate$Maximum_Fatality_Ratio))

library(forcats) #Sorry, I used a slightly different method. I was having a lot of trouble.
state_max_fatality_rate %>%
  as.data.frame() %>% 
  mutate(Province_State = fct_reorder(Province_State, desc(state_max_fatality_rate$Maximum_Fatality_Ratio))) %>% 
  ggplot(aes(x=Province_State, y=Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=90))










###########################
#                         #
#           VI            #
#                         # 
###########################

usa_cumulative_deaths <- covid_df %>% 
  group_by(Last_Update) %>% 
  summarise(sum(Deaths))

# usa_cumulative_deaths %>% 
  # names()[names() == "`sum(Deaths)`"] <- "Total_Deaths"
  # rename(`sum(Deaths)` = Total_Deaths, Last_Update = Time) # Not going to bother, though I realize "`sum(Deaths)`" is not a good column name.

usa_cumulative_deaths %>% 
  ggplot(aes(y=`sum(Deaths)`, x=as.Date(Last_Update))) +
  geom_point() +
  theme_bw() +
  scale_x_date()

