# class notes 1/23/2025

bfile <- list.files(paste0(getwd(), '/Data/'), pattern = '^b', recursive = TRUE)

for (file in bfile) {
  filepath <- paste0(getwd(), '/Data/', file)
  first_line <- readLines(filepath, n = 1)
  
  print(first_line)
}
