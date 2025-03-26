## Assignment 2
## Leah Ellis

library(tidyverse)

# 4. Write a command that lists all of the .csv files found in the
# Data/ directory and stores that list in an object called “csv_files”
csv_files <- paste0(getwd(), '/Data/') %>% list.files() 

# 5. Find how many files match that description using the length() function
csv_files %>% length()

# 6. Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function
df <- read.csv(paste0(getwd(), '/Data/wingspan_vs_mass.csv'))

# 7. Inspect the first 5 lines of this data set using the head() function
head(df, 5)

# 8. Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
b_files <- list.files(paste0(getwd(), '/Data/'), pattern = "^b", recursive = TRUE, full.names = TRUE)

# 9. Write a command that displays the first line of each of those “b” files (this is tricky… use a for-loop)
for (f in b_files) {
  read.csv(f, nrows = 1) %>% print
}

# 10. Do the same thing for all files that end in “.csv”
for (f in list.files(paste0(getwd(), '/Data/'), pattern = ".csv$", recursive = TRUE, full.names = TRUE)) {
  read.csv(f, nrows = 1) %>% print
}
