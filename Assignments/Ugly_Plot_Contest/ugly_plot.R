library(tidyverse)
library(ggplot2)

data("fish_encounters")

view(fish_encounters)

img <- readPNG(system.file("img", "Images/", package="png"))

ggplot(fish_encounters, 
       aes(x = station, y = fish)) +
  geom_line() + 
  theme_bw()
