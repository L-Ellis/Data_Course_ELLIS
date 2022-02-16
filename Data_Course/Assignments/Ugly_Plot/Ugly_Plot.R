install.packages("spiralize")

library(tidyverse)
library(spiralize)



table1


us <- us[us$dt < "2022-01-07",]
ymax <- max(us$new_cases_smoothed)


# Initialize.
spiral_initialize_by_time(xlim=c("2020-01-01 00:00:00", "2022-01-06 00:00:00"),
                          unit_on_axis = "days", period="years",
                          start=90, end=(709/365)*360+(28/365)*360+90,
                          flip="horizontal")

# Create the track.
spiral_track(ylim=c(0, ymax*.7),
             background=FALSE, background_gp = gpar(col = NA, fill = NA))


# Use a polygon.
spiral_polygon(x=c(us$dt, rev(us$dt)),
               y=c(us$new_cases_smoothed/2, -rev(us$new_cases_smoothed/2)),
               gp = gpar(col="#d32e2b", fill="#d32e2b50"))

# Middle baseline.
spiral_lines(x=us$dt, y=0)


# Text.
spiral_text(x="2020-01-01", y=50000, text="2020",
            facing = "curved_inside", just = "right",
            gp=gpar(cex=1, fontfamily="Courier"))
spiral_text(x="2021-01-01", y=50000, text="2021",
            facing = "curved_inside", just = "right",
            gp=gpar(cex=1, fontfamily="Courier"))
spiral_text(x="2022-01-01", y=50000, text="2022",
            facing = "curved_inside", just = "right",
            gp=gpar(cex=1, fontfamily="Courier"))







# Sources 
# https://flowingdata.com/2022/01/10/a-quick-and-easy-way-to-make-spiral-charts-in-r/









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























