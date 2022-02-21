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


# Faceted Graph
dat <- dat %>% filter(Dilution == 0.1) 

  # Alphabetical order but anything starting with weird characters is put at the end. 
  # I do this by adding a special character to the beginning of everything starting with a weird character that will bump it to the end of an alphabetical sort
  # Then, I only use this to order the levels of the actual Substrate data, so the special character is not included in the graph.

non_ASCII_substrate <- dat$Substrate[ grepl(dat$Substrate,pattern="^[^ -~]") ]
non_ASCII_substrate <- paste0("zzzz", non_ASCII_substrate)
  # I mean, I could just use zzzzz or something, but this feels cooler. 
  # https://stackoverflow.com/questions/8086375/what-character-to-use-to-put-an-item-at-the-end-of-an-alphabetic-list

ASCII_substrate <- dat$Substrate[ !grepl(x=dat$Substrate,pattern="^[^ -~]") ]
substrate_order <- unlist(list(non_ASCII_substrate, ASCII_substrate)) # Combine # https://www.geeksforgeeks.org/concatenate-two-given-factor-in-a-single-factor-in-r/

dat <- dat %>% mutate(Substrate = factor(x=Substrate, levels = sort(substrate_order))) # no NA's here, but it's not sorted right.

dat$Substrate %>% view
sort(substrate_order) %>% view
dat$Substrate = factor(dat$Substrate,levels = sort(unique(substrate_order))) %>% view #something wrong with unique??? WHERE ARE THE NA's COMING FROM??

dat %>% 
  ggplot(aes(x=Time,y=Absorbance,color=Sample.Source)) +
  geom_line() +
  facet_wrap(~ Substrate) +
  theme_bw() 


# Animatated Graph

library(gganimate)












  




# ASCII_substrate <- dat$Substrate[ ! grepl(x=unique(dat$Substrate),pattern="^[^ -~]")]
# ASCII_substrate <- factor(ASCII_substrate,levels=sort(unique(ASCII_substrate)))

# non_ASCII_substrate <- dat$Substrate[grepl(x=dat$Substrate,pattern="^[^ -~]")]
# non_ASCII_substrate <- factor(non_ASCII_substrate,levels=sort(unique(non_ASCII_substrate) + (length(unique(ASCII_substrate)) - length(unique(non_ASCII_substrate))))) # place at end

# ordered_substrate <- unlist(list(ASCII_substrate,non_ASCII_substrate)) # https://www.geeksforgeeks.org/concatenate-two-given-factor-in-a-single-factor-in-r/
# levels(ordered_substrate) %>% view


#   group_by(grepl(x=Substrate,pattern="^[^ -~]")) %>% 
#   group_modify(~ mutate(.x, Substrate = factor(Substrate, levels = unique(sort(dat$Substrate))))) %>% 

# sort_ASCII_last <- function(x) {
#   paste0("ZZZZ", x)
# }

# ordered_substrate <- dat$Substrate %>% 
#   apply(MARGIN = 1, FUN=sort_ASCII_last(x=dat$Substrate[grepl(pattern="^[^ -~]")]))
  
# group_by(grepl(x=Substrate,pattern="^[^ -~]")) %>% 
# group_map(~ mutate(Substrate = factor(dat$Substrate, levels = unique(dat$Substrate))), data = .x)

# group by pattern, sort each group alphabetically, merge groups so that levels of group 2 are appended to the end

# group_by anything starting with non-ASCII characters # https://stackoverflow.com/questions/34613761/detect-non-ascii-characters-in-a-string

# mutate(Substrate = factor(x=Substrate,levels=levels(ordered_substrate))) %>% # - levels()


any(is.na(substrate_order))
any(is.na(dat$Substrate))




