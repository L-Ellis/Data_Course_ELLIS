library(tidyverse)

# table1
# table2
# table3
# table4a
# table4b
# table5

table2
t2 <-  table2 %>% 
  pivot_wider(id_cols = c(country,year), # which cols to give rows
              names_from = type, # which col to split
              values_from = count) # where to find values to fill

table3
t3 <-  table3 %>% 
  separate(col=rate, # which col to separate
           into=c("cases","population"), # new cols
           sep="/", # which symbol will be treated as a separator
           convert=TRUE) # turn into int
          

dfa <- table4a %>% 
  pivot_longer(cols = -country, #everything except country
               names_to = "year", #new col name
               values_to = "cases") #new col name for values

dfb <- table4b %>% 
  pivot_longer(cols = -country,
               names_to = "year",
               values_to = "population") 

t4 <-  full_join(x=dfa,y=dfb)


# from: https://github.com/gadenbuie/tidyexplain
# ?full_join
# ?inner_join
# ?left_join
# ?right_join
# ?semi_join
# ?anti_join
# # # # # 

table5
t5 <- table5 %>% 
  separate(col=rate, # which col to separate
           into=c("cases","population"), # new cols
           sep="/", # which symbol will be treated as a separator
           convert=TRUE) %>%  # turn into int 
  unite(col="year", # name of united col
        sep="",
        century,year
        ) %>% 
  mutate(year=as.numeric(year),
         cases=as.numeric(cases),
         population=as.numeric(population))  

table1 == t2 && table1 == t3 && table1 == t4 && table1 == t5


# sex, family, spp, ckutch size, egg mass, mating syst, mass, bill, wing

male <- df %>% 
  select(ends_with("_N")) %>%
  select(starts_with("M_"),Family,Species_number,Species_name, English_name,
  Clutch_size,Clutch_size,Egg_mass,Mating_System) %>% 
  mutate(Sex="Male") %>% 
  select(Sex,Family,Species_number,Species_name,English_name,Clutch_size,Egg_mass,Mating_System)
names(male) <- str_remove_all(names(female),"M_")

female <- df %>% 
  select(ends_with("_N")) %>%
  select(starts_with("F_"),Family,Species_number,Species_name, English_name,
         Clutch_size,Clutch_size,Egg_mass,Mating_System) %>% 
  mutate(Sex="Female") %>% 
  select(Sex,Family,Species_number,Species_name,English_name,Clutch_size,Egg_mass,Mating_System)
names(female) <- str_remove_all(names(female),"F_")


unsexed <- df %>% 
  select(ends_with("_N")) %>%
  select(starts_with("U_"),Family,Species_number,Species_name, English_name,
         Clutch_size,Clutch_size,Egg_mass,Mating_System) %>% 
  mutate(Sex="Unsexed") %>% 
  select(Sex,Family,Species_number,Species_name,English_name,Clutch_size,Egg_mass,Mating_System)
names(unsexed) <- str_remove_all(names(unsexed),"U_")






  
