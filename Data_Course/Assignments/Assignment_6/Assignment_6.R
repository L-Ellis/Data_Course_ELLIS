library(tidyverse)
library(ggplot2)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

# Clean up Hr_* weirdness
dat <- dat %>% 
  pivot_longer(starts_with("Hr_"),
               names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "Hr_",
               names_transform = list(Time = as.numeric)) 

# Fix funky `` names 
names(dat) <- names(dat) %>% 
  make.names(unique = TRUE)

# How many distinct sample.id's are there?
dat %>% 
  distinct(Sample.ID)

# Create a new column that says whether the sample came from soil or water
dat <- dat %>% 
  mutate(Sample.Source = case_when(
      grepl(x = Sample.ID, pattern = "Soil") ~ "Soil", # if
      TRUE ~ "Water" # else 
  ))

# Does the new column work?
dat %>% 
  distinct(Sample.ID, .keep_all = TRUE)


# dat %>% 
  # distinct(Substrate) %>% 
  # nrow()


# Faceted Graph #WIP
dat %>% 
  filter(Dilution == 0.1) %>% 
  
  # Alphabetical order but anything starting with weird characters are put at the end.
  group_by(grepl(x=Substrate,pattern="^[^ -~]")) %>%  # group_by anything starting with non-ASCII characters # https://stackoverflow.com/questions/34613761/detect-non-ascii-characters-in-a-string
  group_modify(
    mutate(dat$Substrate = factor(dat$Substrate, levels = order(dat$Substrate)))
    ) %>% 
  
  ggplot(aes(x=Time,y=Absorbance,color=Sample.Source)) +
  geom_line() +
  facet_wrap(~ Substrate)
  
  

  







