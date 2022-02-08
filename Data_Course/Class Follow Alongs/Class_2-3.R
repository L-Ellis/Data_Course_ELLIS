library(tidyverse)
library(ggimage) # Unused

############################
#                          #
# Unnamed multi-Panel Plot #
#                          # 
############################

# mpg %>% glimpse() # Car Data
mpg %>% 
  ggplot(aes(y=cty,x=displ,colour=factor(year))) + # Naming arguments (x=foo,y=foof) is best practice!
  geom_point() +  
  geom_smooth(method="lm", formula=y~poly(x,2)) + # poly is polynomial. # "~" is "as a function of"
  theme_bw() +
  labs(x="Engine displacement (L)",
       y="City miles per gallon",
       color="Year") + # "color" refers to the color legend
  scale_color_manual(values = c("#FF499E", "#D264B6", "#A480CF", "#779BE7", "#49B6FF")) +
  
  facet_wrap(~drv) +
  theme(strip.text = element_text(face="italic"), strip.background = element_rect(fill = "#A480CF", color = "Black"), 
        axis.title.x = element_text(angle=0),
        plot.background = element_rect(fill = "#779BE7"))


###########################
#                         #
#   Ordered Violin Plot   #
#                         # 
###########################

ordered_by_med <- mpg %>% # Order by the median hwy descending, must be used in mutate() and fed into ggplot later
  group_by(class) %>% 
  summarize(MedHwy = median(hwy)) %>% 
  arrange(MedHwy) # feed in desc(MedHwy) to reverse order

mpg$class %>% class # Similar to mutate()
factor(mpg$class,levels=c("suv","midsize"))

mpg %>% 
  mutate(ordered_class = factor(class,levels=ordered_by_med$class)) %>%  # %>% View()
  ggplot(aes(x=ordered_class,y=hwy)) +
  geom_violin(fill="Purple") +
  geom_jitter() +
  geom_boxplot(alpha=.5) +
  coord_flip()


