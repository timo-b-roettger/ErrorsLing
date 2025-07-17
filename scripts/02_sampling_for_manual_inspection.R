## title: Statistical reporting inconsistencies in Ling - Sampling for manual check
## author: Timo Roettger
## contact: timo.b.roettger@gmail.com   
## date created: 2024-07-11
## date last edited: 2024-07-25


## packages ####################################################################                        

# load in relevant libraries, install if not installed
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(rstudioapi, 
#                tidyverse, 
#                ggpubr
# )

## data ########################################################################                        

## load in data from location of script 
## (uncomment if you run script outside of sourcing it through manuscript.qmd)
#current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(current_working_dir)

## load in data
xdata <- read_csv("../data/statcheck_revised_sample.csv")

## split strings into year and journal, drop NAs
xdata <- xdata %>% 
  mutate(year = strtoi(substring(source, 1, 4)),
         journal = substring(source, 9, 11)) %>%
  drop_na(reported_p) %>%
  drop_na(computed_p) %>%
  filter(year != 2024)

## remove LCN before before 2015 
xdata <- xdata |> 
  filter(! (journal == "LCN" & year < 2015))

# wrangle for sampling
xdata <- xdata |> 
  filter(decision_error == TRUE)

# check distribution
table(xdata$journal, xdata$year)

# many journals have gaps, so we only sample across years

# randomly sample x% of decision inconsistencies
# max one per article
# evenly across years

sample_prop = 0.15
sample_size = nrow(xdata) * sample_prop
# 216
sample_size_per_year = sample_size / length(unique(xdata$year))
# 9
  
# set random seed
set.seed(999)

# first create subset with one test per article
xdata_sample <- xdata |> 
  group_by(source) %>% 
  sample_n(1)

# check if possible
table(xdata_sample$year)

# yes, proceed

# second create subset with the desired tests per year
xdata_sample <- xdata_sample |> 
  group_by(year) %>% 
  sample_n(sample_size_per_year)

# check if duplicate articles
duplicated(xdata_sample$source)
# no

# save list
write_csv(xdata_sample, "../data/subsample/subsample.csv")


# extract files and store them into folder
# folder from which to extract
source_dir <- "../Journals/"
# destination folder
dest_dir <- "../data/subsample/"

# rename file extensions of target files
txt_temp <- gsub(".pdf", "", xdata_sample$source)

# define the list of target filenames to look for
target_files <- paste0(txt_temp, ".txt")  # Example file names

# get all files recursively from the source directory
all_files <- list.files(path = source_dir, recursive = TRUE, full.names = TRUE)

# loop through all files
for (file in all_files) {
  # extract the filename (without path)
  filename <- basename(file)
  
  # if the filename is in the target list
  if (filename %in% target_files) {
    # copy the file to the destination directory
    file.copy(from = file, to = file.path(dest_dir, filename), overwrite = TRUE)
  }
}



