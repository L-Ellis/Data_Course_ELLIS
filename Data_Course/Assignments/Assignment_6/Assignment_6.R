library(tidyverse)
library(ggplot2)


###########################
#                         #
#      Setup Data         #
#                         # 
###########################
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




###########################
#                         #
#     Faceted Graph       #
#                         # 
###########################

dat_plot1 <- dat %>% filter(Dilution == 0.1) # Very important to do before anything else

###########################################################################################################
#   Sort substrates in alphabetical order but any starting with weird characters is put at the end        #
#                                                                                                         #
###########################################################################################################
# I do this by adding characters to the beginning of everything starting with a weird character that will bump it to the end of an alphabetical sort
# Then, after changing the levels of a factor, I remove the added characters to prevent NA's caused by unmatching level name.
# I figure this is kind of an awful way to do this, I was messing around a lot with group_by and group_modify hoping I could sort the groups individually then throw one on top of the other, 
# but I couldn't do it. So I used this long and hacky method instead, sorry.

non_ASCII_substrate <- dat_plot1$Substrate[ grepl(dat_plot1$Substrate,pattern="^[^ -~]") ] # https://stackoverflow.com/questions/34613761/detect-non-ascii-characters-in-a-string
non_ASCII_substrate <- paste0("z", non_ASCII_substrate) 
non_ASCII_substrate[grepl(non_ASCII_substrate,pattern="^zγ")] <- paste0("z", non_ASCII_substrate[grepl(non_ASCII_substrate,pattern="^zγ")])  # further bump γ-Hydroxybutyric Acid to the end because that's where it is on the desired graph
# I mean, I could just use  or something, but for some reason the sorts before everything alphabetically, instead of at the end like it is supposed to? # https://stackoverflow.com/questions/8086375/what-character-to-use-to-put-an-item-at-the-end-of-an-alphabetic-list

ASCII_substrate <- dat_plot1$Substrate[ !grepl(x=dat_plot1$Substrate,pattern="^[^ -~]") ]

# Combine # https://www.geeksforgeeks.org/concatenate-two-given-factor-in-a-single-factor-in-r/
ordered_substrate <- unlist(list(non_ASCII_substrate, ASCII_substrate))

# Sort the new factor
ordered_substrate <- factor(ordered_substrate, levels = sort(unique(ordered_substrate)))

# Remove the added characters
  # I could make the regex a little better, just ask it to remove all repeated z's at the beginning rather than just 1 or 2 # https://stackoverflow.com/questions/1660694/regular-expression-to-match-any-character-being-repeated-more-than-10-times
levels(ordered_substrate)[grepl(x=levels(ordered_substrate),pattern="^z|^zz")] <- sub("^z|^zz","", levels(ordered_substrate)[grepl(x=levels(ordered_substrate),pattern="^z|^zz")]) 


#############
#   Plot    #                                                                                                                 
#############
dat_plot1 %>% 
  mutate(Substrate = factor(x=Substrate, levels = levels(ordered_substrate))) %>% 
  ggplot(aes(x=Time,y=Absorbance,color=Sample.Source)) +
  geom_smooth(method="loess", se=FALSE, fullrange=FALSE, span=1) +
  ylim(0, 2.0) +
  facet_wrap(~ Substrate, scales = "fixed") +
  theme_bw() +
  theme(strip.background = element_rect(fill="white",color = "white"),
        panel.border = element_rect(color = "white"),
        axis.ticks = element_blank()
        ) +
  labs(title = "Just dilution 0.1") + 
  guides(color=guide_legend("Type"))
  

# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels

# The lines look awfully pixelated in the export. I don't know what could be causing that, I'm not sure if it's a problem with how I'm using geom_smooth or not.





###########################
#                         #
#     Animated Graph      #
#                         # 
###########################

library(gganimate)

# Filter
dat_plot2 <- dat %>% 
  filter(Dilution == 0.001 || 0.01 || 0.1) %>%
  filter(Substrate == "Itaconic Acid")
  
# Mean
dat_plot2 <- dat_plot2 %>% 
  group_by(Time, Dilution, Sample.ID) %>% 
  summarise(Mean_absorbance = mean(Absorbance))

# Plot
dat_plot2 %>% 
  ggplot(aes(x=Time,y=Mean_absorbance,color=Sample.ID)) + 
  geom_line() + 
  facet_wrap(~Dilution) +
  theme_bw() +
  theme(strip.background = element_rect(fill="white",color = "white"),
        panel.border = element_rect(color = "white"),
        axis.ticks = element_blank() ) +
  guides(color=guide_legend("Sample ID")) +
  gganimate::transition_reveal(Time)






