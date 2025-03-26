# Leah Ellis - Assignment 6
# 03/25/2025

library(tidyverse)
library(gganimate)

dat <- read_csv("Data/BioLog_Plate_Data.csv")

## Clean Data

df1 <- dat %>% pivot_longer(cols = c('Hr_24', 'Hr_48', 'Hr_144'), names_to = "Time", names_prefix = "Hr_", values_to = "Absorbance") %>% 
  mutate(`Sample Source` = case_when(`Sample ID` == "Soil_1" | `Sample ID` == "Soil_2" ~ "Soil", `Sample ID` == "Clear_Creek" | `Sample ID` == "Waste_Water" ~ "Water")) %>% 
  mutate(across(Time, as.numeric))


## First Plot
# If you're on windows, to get the anti-aliased version of this graph you'd have to use Cairo, but since you're probably using mac or linux I'll just hope it looks the same as the example for you.

df1 %>% filter(Dilution == 0.1) %>%
  mutate(Substrate = factor(Substrate, c("2-Hydroxy Benzoic Acid", "4-Hydroxy Benzoic Acid", "D-Cellobiose", "D-Galactonic Acid γ-Lactone", "D-Galacturonic Acid", "D-Glucosaminic Acid", "D-Mallic Acid", "D-Mannitol",
                                         "D-Xylose", "D.L -\u03B1-Glycerol Phosphate", "Glucose-1-Phosphate", "Glycogen", "Glycyl-L-Glutamic Acid", "i-Erythitol", "Itaconic Acid", "L-Arginine", "L-Asparganine", "L-Phenylalanine",
                                         "L-Serine", "L-Threonine", "N-Acetyl-D-Glucosamine", "Phenylethylamine", "Putrescine", "Pyruvic Acid Methyl Ester", "Tween 40", "Tween 80", "Water", "\u03B1-Cyclodextrin", "\u03B1-D-Lactose", "\u03B1-Ketobutyric Acid",
                                         "\u03B2-Methyl-D- Glucoside", "γ-Hydroxybutyric Acid"))) %>% # I sorted it this way so that it would be ordered identically to the example no matter how your computer deals with non-ascii characters.
  ggplot(aes(x=Time, y=Absorbance, colour=`Sample Source`)) +
  geom_smooth(method="loess", se=FALSE, fullrange=FALSE, span=1) +
  ylim(0, 2.0) +
  facet_wrap(.~Substrate, scales = "fixed") +
  theme_bw() + # take the theme
  theme(strip.background = element_rect(fill="white",colour = "white"), # edit it
        panel.border = element_rect(colour = "white")
  ) +
  labs(title = "Just dilution 0.1") + 
  guides(colour=guide_legend("Type"))

## Animated Plot
df1 %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Time, Dilution, `Sample ID`) %>% 
  summarise(Mean_absorbance = mean(Absorbance)) %>% 
  ggplot(aes(x=Time,y=Mean_absorbance,colour=`Sample ID`)) + 
  geom_line() + 
  facet_wrap(~Dilution) +
  theme_bw() +
  theme(strip.background = element_rect(fill="white",colour = "white"),
        panel.border = element_rect(colour = "white"),
        axis.ticks = element_blank() ) +
  guides(colour=guide_legend("Sample ID")) +
  gganimate::transition_reveal(Time)
