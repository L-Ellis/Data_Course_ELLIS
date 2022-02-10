library(tidyverse)


# filter # chooses based on Boolean expression
# group_by / summarize() 
# mutate # Make new columns based on existing columns.
# arrange # 
# select
iris %>% 
  arrange(desc(Sepal.Length)) %>% 
  select(Sepal.Width)

iris %>% 
  select(contains("Petal")) %>% 
  glimpse()

iris %>% 
  mutate(Sepal.Area = Sepal.Length * Sepal.Width) %>% 
  filter(Species != "setosa") %>% 
  filter(Sepal.Length / Sepal.Width < 2) %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Area,color=Species)) + 
  geom_point()

df <- read_csv("./Data/wide_income_rent.csv")

# income vs rent colored by state
df %>% 
  select()
  glimpse()
  # ggplot(aes(x=,y=,color=))
  

df %>% 
  pivot_longer(-variable, names_to="State",values_to="USD") %>%  # Stacks columns into named variables. Wide data to Long data.
  ggplot(aes(color=variable,x=State,y=USD)) + 
  geom_col(position = "dodge") + # not working? 
  theme(axis.text.x = element_text(angle = 90,hjust=1))


df %>%
  select(starts_with("Pue"))


utah <- read.csv("./Data/Utah_Religions_by_County.csv")

utah %>% 
  # pivot_wider() # Long data to Wide data
  select(Religious, Non.Religious) %>% # Can't include "Religious", as rowsum does not add up to 1 with it. (It also doesn't regardless because of errors in the data)
  rowSums()

utah_clean <- utah %>% 
  pivot_longer(-c("County", "Pop_2010", "Religious"), names_to = "Religion", values_to = "Proportion") 

utah_evangelical <- utah_clean %>% 
  filter(Religion == "Evangelical") %>% 
  arrange(desc(Proportion))

utah_non.religious <- utah_clean %>% 
  filter(Religion == "Non.Religious") %>% 
  arrange(desc(Proportion))

utah_clean %>% 
  mutate(County = factor(County, levels=utah_non.religious$County)) %>% 
  ggplot(aes(x=County,y=Proportion,fill=Religion)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=90,hjust=1))



