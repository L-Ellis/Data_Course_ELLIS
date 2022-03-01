library(tidyverse)
library(tidyr)
library(dplyr)
library(modelr)
library(easystats)
library(GGally)
library(purrr)

airlines = read.csv("airlines.csv")
airports = read.csv("airports.csv")
flights = read.csv("jan_flights.csv")
snowfall = read.csv("Jan_snowfall.csv")

# Class
files = c("airlines.csv", "airports.csv", "jan_flights.csv", "Jan_snowfall.csv")
df = map(files, read.csv)
# 

###################################################################################                                                                  #
#   Combine the data sets appropriately to investigate whether                    #
#    departure delay was correlated with snowfall amount                          #                                                                             #
################################################################################### 
# By Date(Date,YEAR-MONTH-DAY) and by Airport(iata, ORIGIN_AIRPORT)

# Cleanup Dates
flights <- flights %>% 
  unite(col="Date", c(YEAR,MONTH,DAY), sep= "-") %>% 
  mutate(Date = as.Date(Date)) # Add 0's
snowfall <- snowfall %>% 
  mutate(Date = as.Date(Date))
  
df <- flights %>% 
  select(Date, DEPARTURE_DELAY, ORIGIN_AIRPORT) %>%  # Trim down to only what I'm gonna compare
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  rename("iata" = "ORIGIN_AIRPORT") %>% # Rename to what snowfall calls it
  left_join(snowfall)
  
snow_to_delay_model <- glm(data=df,formula=DEPARTURE_DELAY ~ snow_precip_cm)
 # summary(snow_to_delay)
snow_to_delay_correlation <- snow_to_delay$coefficients[2] #correlation coefficient of snowfall amount to flight delay.


# When it matters, I filter out rows with NA DEPARTURE_DELAY's, as they tend to mess things up.
# I checked and NA's seem to only show up when the flight is cancelled. So I really can just filter those out I think.
  # flights %>% filter(CANCELLED == 0 & is.na(DEPARTURE_DELAY)) %>% nrow() > 1 


###################################################################
#                                                                 #
#     Plot average departure delays by state over time            #
#                                                                 #
###################################################################


avg_by_state <- flights %>% 
  select(DEPARTURE_DELAY, ORIGIN_AIRPORT, Date) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  rename("IATA_CODE" = "ORIGIN_AIRPORT") %>% 
  left_join(select(airports, IATA_CODE, STATE)) %>% 
  group_by(STATE, Date) %>% 
  summarise(MEAN_DELAY = mean(DEPARTURE_DELAY)) # MEAN_DELAY has some negative values, but there ARE negative DEPARTURE_DELAY's in the dataset, so that's supposed to be happening, those are just flights that took off early.

avg_by_state %>% 
  ggplot(aes(x=Date,y=MEAN_DELAY)) + 
  geom_line() +
  facet_wrap(~STATE)


###################################################################
#                                                                 #
#    Plot average departure delays by airline over time           #
#                                                                 #
###################################################################

avg_by_airline <- flights %>% 
  select(DEPARTURE_DELAY, AIRLINE, Date) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  rename("IATA_CODE" = "AIRLINE") %>% 
  left_join(airlines) %>% 
  group_by(AIRLINE, Date) %>% 
  summarise(MEAN_DELAY = mean(DEPARTURE_DELAY)) 

avg_by_airline %>% 
  ggplot(aes(x=Date,y=MEAN_DELAY)) + 
  geom_line() +
  facet_wrap(~AIRLINE)



###################################################################
#                                                                 #
#    Plot effect of snowfall on departure and arrival delays      #
#                                                                 #
###################################################################
 
# Wait, isn't the effect is just that model I made earlier?
# Do I just need to make a plot comparing that model with an arrival delay model?

df <- flights %>% 
  select(Date, ARRIVAL_DELAY, DESTINATION_AIRPORT) %>%  # Trim down to only what I'm gonna compare
  filter(!is.na(ARRIVAL_DELAY)) %>% 
  rename("iata" = "DESTINATION_AIRPORT") %>% # Rename to what snowfall calls it
  left_join(snowfall)

snow_to_arrival_delay_model <- glm(data=df,formula=ARRIVAL_DELAY ~ snow_precip_cm)

compare_models(snow_to_delay_model, snow_to_arrival_delay_model) %>% plot()

# Well, that's that. 
# I don't really think that's what I'm supposed to do, given that it's just two points and some error bars.

# I'll try making a bar chart, just to be thorough.


# , MEAN_DELAY = factor(MEAN_DELAY , levels = sort(unique(MEAN_DELAY), decreasing = TRUE)

avg_departure <- flights %>% 
  select(DEPARTURE_DELAY, ORIGIN_AIRPORT, Date) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  rename("iata" = "ORIGIN_AIRPORT") %>% 
  left_join(snowfall) %>%  #left_join because merge would take a painful ten seconds each.
  group_by(Date) %>% 
  summarise(MEAN_DELAY = mean(DEPARTURE_DELAY), snow_precip_cm = mean(snow_precip_cm)) %>% 
  mutate(GOING = "departure") %>% 
  pivot_longer(c("MEAN_DELAY", "snow_precip_cm"), names_to = "var", values_to = "val")  # I am dumb, so this is the only way I can figure out how to group by two columns 

avg_arrival <- flights %>% 
  select(ARRIVAL_DELAY, DESTINATION_AIRPORT, Date) %>% 
  filter(!is.na(ARRIVAL_DELAY)) %>% 
  rename("iata" = "DESTINATION_AIRPORT") %>% 
  left_join(snowfall) %>% 
  group_by(Date) %>% 
  summarise(MEAN_DELAY = mean(ARRIVAL_DELAY), snow_precip_cm = mean(snow_precip_cm)) %>% 
  mutate(GOING = "arrival") %>% 
  pivot_longer(c("MEAN_DELAY", "snow_precip_cm"), names_to = "var", values_to = "val")

avg_delay <- rbind(avg_departure, avg_arrival)

avg_delay %>% 
  ggplot(aes(x=Date, y = val, fill = var)) +
  geom_col(position = "dodge") +
  facet_wrap(~GOING) +
  labs(title = "Effect of Snowfall on Arrival and Departure Delays", y = "Amount", fill = "")

# ok, so I sort of gave up a little here. I'm on hour three and am absolutely hitting a brick wall.
# what I WANTED to do was have a grouped bar chart with two subgroups,
# which I was ALMOST able to get by just throwing in foo = GOING into aes,
# but for the life of me I could not figure out how to access this new group to change the colors of the two or provide any labels.
# Which made for a really unreadable chart. It's not like the current indented chart shows what I want either,
# as the tiny differences between are kind of hard to spot with the two graphs split up like that.

# Honestly, I still don't really understand the task at hand. 
# Was I supposed to show the snowfall amount and delays side by side like I do here? 
# Graph the change in the two correlation coefficients over time? Not over anything, just the two points and variances on a graph?
# Sorry I'm really just making something over-complicated, as always. I hope one of the things I made here was what you meant.
  
# https://stackoverflow.com/questions/8134605/how-to-overlay-two-geom-bar
# https://stackoverflow.com/questions/18774632/how-to-produce-stacked-bars-within-grouped-barchart-in-r

avg_delay %>% 
    ggplot(aes(x=Date, y = val, fill = var, fill2 = GOING)) + # Splits the bars up right
    geom_col(position = "dodge") +
    scale_fill_manual(values = case_when(avg_delay$var == "MEAN_DELAY" ~ "royal blue", TRUE ~ "orange")) + # Doesn't color right
    labs(title = "Effect of Snowfall on Arrival and Departure Delays", y = "Amount", fill = "", subtitle = "Arrival delays are the left half of the bar, departure delays are the right")
# fill = c(var, GOING)[order(c(seq_along(var)*2 - 1, seq_along(GOING)*2))]))