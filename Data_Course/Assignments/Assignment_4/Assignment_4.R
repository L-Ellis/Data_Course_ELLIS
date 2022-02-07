# Libraries
library(tidyverse)
library(ggplot2)

###########################
#                         #
#  Generate Example Data  #
#                         # 
###########################
firearm_selection_dir <- "Data/Firearm_Selection.csv"
firearm_selection <- read.csv(firearm_selection_dir, fileEncoding = 'UTF-8-BOM')
firearm_selection

media_selection_dir <- "Data/Media_Selection.csv"
media_selection <- read.csv(media_selection_dir, fileEncoding = 'UTF-8-BOM')
media_selection

example_data <- list("Media_Title" = character(), "Firearms" = list()) # List with nested list
# Format data into the list with nested list structure, with randomized firearms per media
for (x in 1:nrow(media_selection)) {
  firearms <- list();
  for (y in 1:media_selection$Munitions_Number[x]) { 
    # ALERT: the Munitions_Number is always multiplied by 2 somehow? Doesn't break anything yet.
    random <- sample(1:nrow(firearm_selection),1);
    firearms <- append(firearms, firearm_selection[random,]);
  }
  example_data$Media_Title[x] <- media_selection$Media_Selection[x];
  example_data$Firearms[[x]] <- firearms;
}
str(example_data);
# ?list_modify  # for combining duplicates? duplicate firearm models won't be a problem in the actual dataset, presumably

# Problems (example specific): 
# - Multiplied media_selection$Munitions_Number.
# - The randomizer has no regard for duplicates, which won't exist in the true dataset.


###########################
#                         #
#     Example Graph       #
#     (UNFINISHED)        # 
###########################

install.packages("treemap", dependencies = TRUE)
library(treemap)
library(data.table)
dfs <- lapply(example_data, data.frame, stringsAsFactors = FALSE)
df_allMedia <- rbindlist(example_data[2], fill=TRUE)
?rbindlist

df_allMedia <- as.data.frame(example_data)

?treemap

treemap() %>% 
  dtf = 









### Sources:
# https://r4ds.had.co.nz/vectors.html
# https://www.roelpeters.be/removing-i-umlaut-two-dots-data-frame-column-read-csv/
# https://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe



### Junk:
#for (x in 0:nrow(media_selection)) {
#  for (y in media_selection$Munitions_Number[x]) {
#    firearms <- list(); f <- NULL; random <- 0;
#    while(f == NULL || grepl(pattern=as.character(f$Firearm_Name[random,]),x=firearms$Firearm_Name)) {
#      random <- sample(0:nrow(firearm_selection),1)
#      f <- firearm_selection[random,]
#    }
#    firearms <- append(firearms,f)
#  }
#  firearms
#  # example_data <- append(example_data,firearms)
#}






