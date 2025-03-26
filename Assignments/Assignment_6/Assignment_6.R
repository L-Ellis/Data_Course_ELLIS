library(tidyverse)

dat <- read_csv("Data/BioLog_Plate_Data.csv") # you may need to modify this path to fit where this file is stored relative to your assignment 6 script

dat %>% pivot_longer(cols = c('Hr_24', 'Hr_48', 'Hr_144'), names_to = "Time (hours)", values_to = c('24', '48', '144')) %>% view
