# Assignment 7 messy code
# Change this to "tidy" format using dplyr verbs

# There's an intuitive dplyr version for almost everything you see here.

# Note: Do not erase the original code, just comment it out and put your own equivalent code below each section
# i.e., change each line of indicated code to a tidy version that does the same thing.


library(tidyverse)
library(ggplot2)

##########################
#        Part 1          #
##########################

# load data (wide format)
utah = read.csv("./Utah_Religions_by_County.csv")

# # subset to only counties with buddhists observed
# buddhist = utah[utah$Buddhism.Mahayana > 0,]
# 
# # order rows by population (descending)
# buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]

# create buddhist dataframe and order by population (decreasing)
buddhist <- utah %>% 
  filter(Buddhism.Mahayana > 0) %>% 
  arrange(Pop_2010, decreasing = TRUE)

# write buddhist dataframe to csv file
write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)

# ## get group summaries of religiousity based on population ##
# 
# # divide each county into one of six groups based on populations
# # note: keep these two lines the same in your updated code!
groups = kmeans(utah$Pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$Pop.Group = groups$cluster # assigns a new variable to utah giving group for each county
 
# # subset to each group and find summary stats on Religiosity for each
# group1 = mean(utah[utah$Pop.Group == 1,]$Religious)
# group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
# group3 = mean(utah[utah$Pop.Group == 3,]$Religious)
# group4 = mean(utah[utah$Pop.Group == 4,]$Religious)
# group5 = mean(utah[utah$Pop.Group == 5,]$Religious)
# group6 = mean(utah[utah$Pop.Group == 6,]$Religious)
# 
# # same, but mean population
# group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
# group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
# group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
# group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
# group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
# group6.pop = mean(utah[utah$Pop.Group == 6,]$Pop_2010)
# 
# 
# # make data frame of each group and mean religiosity
# religiosity = data.frame(Pop.Group = c("group1","group2","group3","group4","group5","group6"),
#                          Mean.Religiosity = c(group1,group2,group3,group4,group5,group6),
#                          Mean.Pop = c(group1.pop,group2.pop,group3.pop,group4.pop,group5.pop,group6.pop))
# 
# 
# 
# 
# religiosity # take quick look at resulting table
# 
# # order by decreasing population
# religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]
# 
# 
# religiosity # take quick look at resulting table

# group by and summarise for the mean of Religiosity and the mean of 2010 Population, and then arrange by decreasing population
religiosity <- utah %>% 
  group_by(Pop.Group) %>% 
  summarise(Mean.Religiosity = mean(Religious), Mean.Pop = mean(Pop_2010), .groups = 'drop') %>% 
  arrange(Mean.Pop, decreasing = TRUE)

# # plot that table (redo this using ggplot)
# plot(x=religiosity$Mean.Pop,y=religiosity$Mean.Religiosity)

# plot that table (I also added labels for the fun of it :3 )
religiosity %>% 
  ggplot(aes(x=Mean.Pop, y=Mean.Religiosity)) + geom_line() + geom_point() + geom_text(aes(label = round(Mean.Religiosity, 2)), nudge_y = 0.01)



#####################################
#              Part 2               #
# Beginning to look at correlations #
# run this code without changing it #
# it's already in very tidy form    #
#####################################

# Look for correlations between certain religious groups and non-religious people
religions = names(utah)[-c(1:4)]

utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  ggplot(aes(x=Proportion,y=Religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~Religion,scales = "free") + theme_bw() + theme(panel.grid = element_blank(), strip.background = element_rect(fill="Gray"))


# Look through those plots and answer the following questions:
# 1.  Which religious group correlates most strongly in a given area with the proportion of non-religious people?

  # Any graph showing a downwards trend on the X axis is correlated with decreased overall religiosity. 
  # Some of these graphs looks very similar- I'll analyze them with lm to make sure I choose the right one.
 
utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  group_by(Religion) %>%
  do(lm.cf = as.numeric(coef(lm(Proportion ~ Religious,.)))[2]) %>%
  summarise(Religion=Religion, lm.cf=lm.cf, .groups = 'drop') %>% 
  arrange(lm.cf, decreasing = TRUE) %>% 
  print()
  
  # According to this analysis,
  # Evangelicals appear to have the largest correlation with counties with overall decreased religiosity. 

# 2.  What is the direction of that correlation?
  # A negative correlation with overall religiosity. 

# 3.  What can you say about the relationships shown here?
  # Usually, the larger nation-wide christian denominations are more present in counties with decreased overall religiosity.

# 4.  Examine the axis scales. How could you modify the code above to more accurately portray values on an "equal footing?"
  # Use an X limit. 0 to 0.75 works. Just because it is an equal graph does not necessarily make it a good looking one.

utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  ggplot(aes(x=Proportion,y=Religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1),x=c(0,0.75)) +
  facet_wrap(~Religion,scales = "free") + theme_bw() + theme(panel.grid = element_blank(), strip.background = element_rect(fill="Gray"))


# UPLOAD YOUR ANSWERS TO CANVAS
# DON'T FORGET TO PUSH YOUR TIDY CODE TO GITHUB AS WELL!