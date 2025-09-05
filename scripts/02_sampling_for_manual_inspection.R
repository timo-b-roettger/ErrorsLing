## title: Statistical reporting inconsistencies in Ling - Sampling for manual check
## author: Timo Roettger
## contact: timo.b.roettger@gmail.com   
## date created: 2024-07-11
## date last edited: 2024-07-25


## packages ####################################################################                        

# load in relevant libraries, install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rstudioapi,
               tidyverse,
               ggpubr
)

## data ########################################################################                        

## load in data from location of script 
## (uncomment if you run script outside of sourcing it through manuscript.qmd)
# current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(current_working_dir)

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
xdata_de <- xdata |> 
  filter(decision_error == TRUE)

# check distribution
table(xdata_de$journal, xdata_de$year)

# many journals have gaps, so we only sample across years

# take into account comparison sign. Decision errors with = are more difficult to interprete
table(xdata_de$p_comp)
table(xdata_de$p_comp, xdata_de$year)

# should focus only on comparisons that are not equal
xdata_without_equal <- xdata_de |> 
  filter(p_comp != "=")

# randomly sample x% of decision inconsistencies
# max one per article
# evenly across years

sample_prop = 0.15
sample_size = nrow(xdata_de) * sample_prop
# 184
sample_size_per_year = sample_size / length(unique(xdata$year))
# desired sample per year ~7-8
  
# set random seed
set.seed(911)

# first create subset with one test per article
xdata_sample <- xdata_without_equal |> 
  group_by(source) %>% 
  sample_n(1)

# check if possible
table(xdata_sample$year)

# except for 2004 & 2021, yes

# second create subset with the desired tests per year
xdata_sample <- xdata_sample |> 
  group_by(year) %>% 
  # sample the desire number if possible, else sample all from the year
  sample_n(min(n(), sample_size_per_year))

# check if duplicate articles
duplicated(xdata_sample$source)
# no, proceed

# check
table(xdata_sample$year)

# divide into 2
manual_rate_n <- nrow(xdata_sample)/2
rated = as.factor(c(rep("TR", manual_rate_n),rep("DE", manual_rate_n)))

# save list
# write_csv(xdata_sample, "../data/subsample/subsample.csv")
# 
# # save simpler list for manual work
  xdata_sample |>
  ungroup() |> 
  select(source, p_comp, computed_p, raw, one_tailed_in_txt) |>
  mutate(sentence = "copy paste sentence",
         reported_as = "significant/not significant/unclear",
         comment = "add comments",
         rated = as.factor(rated)) |>
  write_csv("../data/subsample/subsample_for_manual_annotation.csv")

# extract files and store them into folder
# folder from which to extract
# source_dir <- "../Journals/"
# # destination folder
# dest_dir <- "../data/subsample/"
# 
# # rename file extensions of target files
# txt_temp <- gsub(".pdf", "", xdata_sample$source)
# 
# # define the list of target filenames to look for
# target_files <- paste0(txt_temp, ".txt")  # Example file names
# 
# # get all files recursively from the source directory
# all_files <- list.files(path = source_dir, recursive = TRUE, full.names = TRUE)
# 
# # loop through all files
# for (file in all_files) {
#   # extract the filename (without path)
#   filename <- basename(file)
#   
#   # if the filename is in the target list
#   if (filename %in% target_files) {
#     # copy the file to the destination directory
#     file.copy(from = file, to = file.path(dest_dir, filename), overwrite = TRUE)
#   }
# }

# read manually annotated version
annotated <- read_csv("../data/subsample/subsample_for_manual_annotation_merged.csv")

# transform consistency of reported significance   
annotated <- annotated |> 
  mutate(computed_significance = case_when(
                                          computed_p > 0.05 ~ "significant", 
                                          computed_p < 0.05 ~ "not significant"),
         match = case_when(
           computed_significance ==  reported_as ~ "match", 
           reported_as == "unclear" ~ "unclear",
           .default = "mismatch"))
# count
matches <- table(annotated$match)[[1]]
mismatches <- table(annotated$match)[[2]]
unclear <- table(annotated$match)[[3]]

