## class notes 1/23/2025

  # install.packages("dplyr")
library(dplyr)

bfile <- list.files(paste0(getwd(), '/Data/'), pattern = '^b', recursive = TRUE)

for (file in bfile) {
  filepath <- paste0(getwd(), '/Data/', file)
  first_line <- readLines(filepath, n = 1)
  
  print(first_line)
}

  # dat <- read.csv('Data/1620_scores.csv')
  # dim(dat)

## build a dataframe for 'mtcars' dataset
df <- data.frame(mtcars)
df[df$cyl > 4,] %>% View()

str(df)

df[, c("mpg", "cyl")]
df[, c(1:2)]

## convert 'mpg' to character in mtcars data frame

df2 <- as.character(df$mpg) 
df2 %>% View

## convert entire data frame to character

df3 <- as.character(df)
df3 %>% View


