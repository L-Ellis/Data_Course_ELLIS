library(tidyverse)
library(palmerpenguins)

penguins %>% names

penguins %>%
  ggplot(aes(x=species,y=body_mass_g,color=body_mass_g)) +
  geom_jitter()

?case_when

penguins <- penguins %>% 
  mutate(opinion = case_when(body_mass_g > 5000 ~ "chonky",
                             body_mass_g <= 5000 ~ "chonk negative"))

penguins %>%
  ggplot(aes(x=species,y=body_mass_g,color=opinion)) +
  geom_jitter()



df = read.csv("./Data/BioLog_Plate_Data.csv")
df <- df %>% 
  pivot_longer(starts_with("Hr_"),names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "Hr_",
               names_transform = list(Time = as.numeric)) # This line is a diff method then used in class, neither is better than the other really.

               
table1 %>% 
  filter(year == 2000 & cases > 3000) %>% # && does not work here for some reason? # ?&&
  select(-year) %>% 
  mutate(rate = (cases/population)*100) %>% 
  arrange(desc(rate))

# Tiebreaker
# arrange(rate,population) # arrange first by rate, then by population in the event of a tie




# Cleaning examples round 2:

table2 %>% 
  pivot_wider(id_cols = c(country,year), # what to leave behind
              names_from = type, # which col has the new variables
              values_from = count) # which col has the values
  

table3 %>% 
  separate(rate,into = c("cases","population")) # / is the sep by default


full_join(
  table4a %>% 
    pivot_longer(cols = -country, names_to = "year", values_to = "cases"),
  table4b %>% 
    pivot_longer(cols = -country, names_to = "year", values_to = "population")
)


library(janitor)
# janitor::excel_numeric_to_date()


library(gganimate)

df = read.csv("./Data/BioLog_Plate_Data.csv")
df <- df %>% 
  pivot_longer(starts_with("Hr_"), names_to = "time", values_to = "absorbance",names_prefix = "Hr_", names_transform = list(time = as.numeric)) %>% 
  filter(Substrate=="L-Arginine")

df %>% 
  ggplot(aes(x=time,y=absorbance)) + 
  geom_point() + 
  facet_wrap(~Rep) +
  gganimate::transition_reveal(time)


# Clean bird data later



