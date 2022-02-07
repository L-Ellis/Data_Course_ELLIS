# Directory path of our Data folder
dataPath <-  c("./Data");

# List of all of the .csv files in the Data directory
dataFiles.csv <- list.files(path = dataPath, pattern = ".csv$", full.names = TRUE, recursive = TRUE);

# The total amount of .csv files in the Data directory 
dataFiles.csv.amount <- length(dataFiles.csv);

# The file that will be read, only one match is expected
targetFile <- grep("wingspan_vs_mass.csv", dataFiles.csv, value = TRUE);

# The contents of that file
df <- read.csv(targetFile[1]); 

# Inspect the first 5 lines of that contents
head(df, n = 5L);

# Find all files in the Data directory that start with "b"
dataFiles.b <- list.files(path = dataPath, pattern = "^b", full.names = TRUE, recursive = TRUE);

# Function that returns the first line of each file in a list
for(i in dataFiles.b){
  print(readLines(i)[1])
}























