# manually loop through txt files
library(tidyverse)
library(readtext)

process_txt <- function(file_path) {
  text <- readtext(file_path)
  statcheck(text$text, OneTailedTxt = TRUE)
}


x <- readtext("../Journals/APS/2000-05-APS-S014271640000106Xa.txt")

txt_files <- list.files(path = "../Journals/BLC", 
                        pattern = "\\.txt$", 
                        full.names = TRUE, 
                        recursive = TRUE)

# Apply the function to each file and combine results
all_data <- map_dfr(txt_files[1:39], process_txt)
