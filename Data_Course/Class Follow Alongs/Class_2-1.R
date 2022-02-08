library(tidyverse)
library(palmerpenguins)
penguins %>% glimpse


ggplot(penguins,aes(x=bill_length_mm,fill=species)) +
  geom_histogram(alpha=.5)



# theme_set(theme(axis.text.x = element_text(angle=60,hjust=1))) # Automatically set them for all future plots


# Not all chart types work with both X and Y



# Body Mass vs Bill Length
pal <- c("#FF499E", "#D264B6", "#A480CF", "#779BE7", "#49B6FF")

ggplot(penguins,aes(x=body_mass_g, y=bill_length_mm)) +
  geom_point(aes(color=species), alpha = .5, size = 20) +
  geom_smooth(method="lm", aes(group=species), color="black", se=FALSE) + # or aes(linetype=species) if you want the lines to look different
  labs(title="B\nE\nE\nP",x="Body Mass (g)", y="Bill Length (mm)", color="Species") + # Change labels. color= changes legend
  theme_minimal() + # Changes background
  theme(plot.title = element_text(hjust=0.5)) + # Change margins?
  # scale_color_viridis_d(option = 10) # scale_color_grey to see what it looks like in greyscale
  scale_color_manual(values = pal) # for creating new color pallets


ggsave("Scripts/myplot2.png", width=6, height=5, dpi=300) # save image to directory


