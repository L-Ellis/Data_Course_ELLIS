# library(dplyr)
# bind_rows(lapply("./Data/practice_data_1.csv", myFun))

library(tidyverse)
library(palmerpenguins)

library(extrafont) 


data("Bird_Measurements")

loadfonts(device = "win") # only for extrafonts # Didn't work

p <- penguins %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g,color=species))


p + 
geom_hline(size = 10, yintercept = 20) +
facet_grid(rows = 9999999998)+ 
geom_point(alpha=1) + 
coord_cartesian(xlim=c(40,50), ylim = c(3000,4000)) +
theme(axis.title.x  = element_text(face = "italic", size = 100, family = "Comic Sans", colour = "magenta")) +
labs(x="Escaped Penguins", y = "Escaped Penguins", title = "The Facility", subtitle = "It's Data Time", caption = "This is HIGH QUALITY Data")








