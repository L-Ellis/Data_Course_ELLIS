library(tidyverse)
library(tidyr)
library(dplyr)
library(modelr)
library(easystats)
library(GGally)

airlines = read.csv("airlines.csv")
airports = read.csv("airports.csv")
flights = read.csv("jan_flights.csv")
snowfall = read.csv("Jan_snowfall.csv")

###################################################################################                                                                  #
#   Combine the data sets appropriately to investigate whether                    #
#    departure delay was correlated with snowfall amount                          #                                                                             #
################################################################################### 
# By Date(Date,YEAR-MONTH-DAY) and by Airport(iata, ORIGIN_AIRPORT)

# Cleanup Dates
flights <- flights %>% 
  unite(col="Date", c(YEAR,MONTH,DAY), sep= "-") %>% 
  mutate(Date = as.Date(Date)) # Add 0's
  
df <- flights %>% 
  select(Date, DEPARTURE_DELAY, ORIGIN_AIRPORT) %>%  # Trim down to only what I'm gonna compare
  rename("iata" = "ORIGIN_AIRPORT") %>% # Rename to what snowfall calls it
  merge(snowfall) %>% 
  filter(!is.na(DEPARTURE_DELAY))

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
  rename("IATA_CODE" = "ORIGIN_AIRPORT") %>% 
  merge(select(airports, IATA_CODE, STATE)) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
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
  rename("IATA_CODE" = "AIRLINE") %>% 
  merge(airlines) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  group_by(AIRLINE, Date) %>% 
  summarise(MEAN_DELAY = mean(DEPARTURE_DELAY)) 

avg_by_airline %>% 
  ggplot(aes(x=Date,y=MEAN_DELAY)) + 
  geom_line() +
  facet_wrap(~AIRLINE)



###################################################################
#                            WIP                                  #
#    Plot effect of snowfall on departure and arrival delays      #
#                            WIP                                  #
###################################################################

# 

avg_departure_delay_by_snowfallamount <- flights %>% 
  select(DEPARTURE_DELAY, ORIGIN_AIRPORT, Date) %>% 
  rename("iata" = "ORIGIN_AIRPORT") %>% 
  merge(snowfall) %>% 
  filter(!is.na(DEPARTURE_DELAY)) %>% 
  group_by(Date) %>% 
  summarise(MEAN_DELAY = mean(DEPARTURE_DELAY)) 
  

  # I assume I now need to take into account the destination airport instead

avg_arrival_delay_by_snowfallamount <- flights %>% 
  select(ARRIVAL_DELAY, DESTINATION_AIRPORT, Date) %>% 
  rename("iata" = "DESTINATION_AIRPORT") %>% 
  merge(snowfall) %>% 
  filter(!is.na(ARRIVAL_DELAY)) %>% 
  group_by(Date, snow_precip_cm) %>% 
  summarise(MEAN_DELAY = mean(ARRIVAL_DELAY)) 

avg_arrival_delay_by_snowfallamount %>% 
  ggplot(aes(x=snow_precip_cm,y=MEAN_DELAY)) + 
  geom_line()



