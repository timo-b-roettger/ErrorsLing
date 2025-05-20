## title: Statistical reporting inconsistencies in Ling
## author: anonymous
## contact: anonymous   
## date created: 2024-07-11
## date last edited: 2024-07-25


## packages ####################################################################                        

# load in relevant libraries, install if not installed
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(rstudioapi, 
#                tidyverse, 
#                ggpubr, 
#                statcheck,
#                janitor,
#                patchwork,
#                ggstream
# )

library(rstudioapi)
library(tidyverse)
library(ggpubr)
library(statcheck)
library(janitor)
library(patchwork)
library(ggstream)


## data ########################################################################                        


## load in data from location of script 
## (uncomment if you run script outside of sourcing it through manuscript.qmd)
#current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(current_working_dir)

## list all journal units
## only works with access to all article pdfs, skip if you don't have access
files <- list.files("../Journals", recursive = TRUE, pattern = "\\.txt")

## how many articles?
length(files)
# 13065

## summary table
files_wrangled <- as.data.frame(files) %>%
  mutate(year = substring(files, 5, 8)) %>%
  filter(year != 2024) %>%
  mutate(journal = substring(files, 13, 15)) %>%
  group_by(journal) %>%
  summarise(n.art = length(unique(files)))

## view
files_wrangled

## store for later
n_articles = length(files)


## statcheck ###################################################################                      

## read pdfs in /Journals with statcheck, checks for one tailed tests
## can only be run with access to original articles, bulk sharing is not prohibited by the journals
#stat <- checkPDFdir("Journals", OneTailedTxt = TRUE)

## write statcheck data to csv
#write.csv(stat, "../data/statcheck_data_second_round.csv", row.names = FALSE)


## data ########################################################################                        

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


## summary tables ##############################################################                       

## summary table counts (table 1)
journal_summary_counts <- xdata %>% 
  group_by(journal) %>% 
  summarise( 
    checked_articles = length(unique(source)),
    checked_results = n(),
    errors = sum(error),
    gross_errors = sum(decision_error)
  ) %>% 
  full_join(files_wrangled) %>% 
  adorn_totals(name = "Total") %>% 
  select(journal, n.art, checked_articles, checked_results, errors, gross_errors)

colnames(journal_summary_counts) <- c("Journal", "eligible articles", "assessable articles",
                                      "assessable results", "inconsistencies", "decision inconsistencies")

# less accessibility in journals without explicit APA guidelines?
journal_summary_counts |> 
  mutate(sample = ifelse(Journal %in% c("JML", "LCN", "JPR", "SLH", "BAL"), "new", "old"),
         accessibility = `assessable articles` / `eligible articles`) |> 
  group_by(sample) |> 
  summarise(mean_assess = mean(accessibility))
# virtually identical

# prevalence of errors for different comparison signs
sign_distribution <- xtabs(~error + p_comp, xdata)
sign_distribution_decision <- xtabs(~decision_error + p_comp, xdata)

## summary table proportions
journal_summary_prop <- xdata %>% 
  group_by(journal) %>% 
  summarise( 
    checked_articles = length(unique(source)),
    checked_results = n(),
    errors = sum(error),
    gross_errors = sum(decision_error),
    prop_errors = (errors / checked_results) * 100,
    prop_gross = (gross_errors / checked_results) *100) %>% 
  full_join(files_wrangled) %>% 
  group_by(journal) %>% 
  mutate(prop_articles =  (checked_articles / n.art) * 100) %>% 
  select(-checked_articles, -checked_results, -errors, -gross_errors, -n.art) %>% 
  adorn_totals(name = "Mean") %>%
  mutate(across(where(is.numeric), 
                ~ replace(., n(), .[n()]/(n()-1)))) 

## now proportion of checkable articles that contain 1 or more errors and 1 or more decision errors
per_article <- xdata %>% 
  group_by(journal, source) %>% 
  summarise(checked_results = n(),
            errors = sum(error),
            gross_errors = sum(decision_error),
            has_error = ifelse(errors > 0, 1, 0),
            has_gross = ifelse(gross_errors > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(journal) %>% 
  summarise(prop_has_error = mean(has_error),
            prop_has_gross = mean(has_gross)
  )

## overall
gross_per_article <- xdata %>% 
  group_by(source) %>% 
  summarise(checked_results = n(),
            errors = sum(error),
            gross_errors = sum(decision_error),
            has_error = ifelse(errors > 0, 1, 0),
            has_gross = ifelse(gross_errors > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  summarise(prop_has_error = mean(has_error),
            prop_has_gross = mean(has_gross)
  )

## per_article into long format for plot
per_article_long <- per_article %>% 
  pivot_longer(!journal, names_to = "type", values_to = "proportion")


## Figure 1 ####################################################################                       

## plot as stacked barplot (figure 1)
stacked_bar <- 
  ggplot(per_article) +
  geom_bar(aes(x = reorder(journal, prop_has_error), 
               y = 100),
           stat = "identity")  +
  geom_bar(aes(x = journal, 
               y = 100),
           fill = "lightgrey",
           stat = "identity")  + 
  geom_bar(aes(x = reorder(journal, prop_has_error), 
               y = prop_has_error * 100),
           fill = "#6002ee",
           stat = "identity") +  
  geom_text(aes(x = reorder(journal, prop_has_error), 
                y = 30,
                label = paste0(round(prop_has_error * 100, 0), "%")),
            color = "white") +
  geom_bar(aes(x = reorder(journal, prop_has_error), 
               y = prop_has_gross * 100),
           fill = "#ee6002",
           stat = "identity") +
  geom_text(aes(x = reorder(journal, prop_has_error), 
                y = 5,
                label = paste0(round(prop_has_gross * 100, 0), "%")),
            vjust = 0,
            color = "white") +
  scale_fill_manual(name = "",
                    values = c("prop_has_error" = "#6002ee", "prop_has_gross" = "#ee6002"),
                    labels = c("inconsistency", "decision inconsistency")) +
  labs(x = "\njournal",
       y = "proportion of articles \nwith at least one inconsistency\n") + 
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal() 

## save
ggsave(filename = "../plots/figure1.png",
       device = "png",
       bg = "white",
       width = 180, 
       height = 120,
       units = "mm", 
       dpi = 300) 

## Figure 2 ####################################################################                        

## aggregate for proportion over year
year_summary_prop <- xdata %>% 
  group_by(year) %>% 
  summarise( 
    checked_articles = length(unique(source)),
    checked_results = n(),
    errors = sum(error),
    gross_errors = sum(decision_error),
    prop_errors = errors / checked_results,
    prop_gross = gross_errors / checked_results
  ) 

year_summary_prop_long <- year_summary_prop %>% 
  mutate(all = .5) %>% 
  # hack to scale streamplot correctly to 50%
  mutate(prop_errors = prop_errors *2,
         prop_gross = prop_gross *2) %>%
  select(year, prop_errors, prop_gross, all) %>% 
  pivot_longer(!year, names_to = "error", values_to = "proportion")
  
## aggregate for proportion over year per journal
year_journal_prop <- xdata %>% 
  group_by(year, journal) %>% 
  summarise( 
    checked_articles = length(unique(source)),
    checked_results = n(),
    errors = sum(error),
    gross_errors = sum(decision_error),
    prop_errors = errors / checked_results,
    prop_gross = gross_errors / checked_results
  ) 

year_journal_prop_long <- year_journal_prop %>% 
  mutate(all = 0.5) %>%
  # hack to scale streamplot correctly to 50%
  mutate(prop_errors = prop_errors *2,
         prop_gross = prop_gross *2) %>%
  select(year, journal, prop_errors, prop_gross, all) %>% 
  pivot_longer(c(-year, -journal), names_to = "error", values_to = "proportion")


## plot prop over time
year_plot <- 
  ggplot(year_summary_prop_long, 
         aes(x = year, y = proportion, fill = error)) +
  geom_stream(bw = 0.7,
              type = "proportion")  +
  scale_fill_manual(values = c("lightgrey","#6002ee","#ee6002"),
                    name = "") +
  annotate("label",
           label = c("inconsistency", "decision inconsistency"),
           x = c(2001, 2001),
           y = c(0.95, 0.86),
           fill = c("#6002ee","#ee6002"),
           color = c("white", "white"),
           fontface = "bold",
           label.padding = unit(0.5, "lines"),
           size = 3,
           hjust = 0) + 
  labs(x = "\nyear of publication",
       y = "proportion of inconsistencies per article\n") +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0%", "10%", "20%", "30%", "40%",  "50%")
  ) +
  scale_x_continuous(breaks =  c(2000, 2005, 2010, 2015, 2020, 2023)) +
  theme_minimal() + 
  theme(legend.position = "none")

## plot prop over time per journal
year_journal_plot <- 
  ggplot(year_journal_prop_long, 
         aes(x = year, y = proportion, fill = error)) +
  geom_stream(bw = 0.7,
              type = "proportion")  +
  scale_fill_manual(values = c("lightgrey","#6002ee","#ee6002"),
                    name = "") +
  facet_wrap(~ journal, ncol = 3) +
  labs(x = "\nyear of publication",
       y = "") +
   scale_y_continuous(breaks = c(0,.5,1),
                      labels = c("0%", "25%", "50%")
                      ) +
  scale_x_continuous(breaks =  c(2000, 2023),
                     labels = c("'00", "'23")) +
  theme_minimal() + 
    theme(legend.position = "none",
          panel.spacing.x = unit(1, "lines"))

## combine (figure 2)
year_p <- year_plot + year_journal_plot
year_p

## save
ggsave(filename = "../plots/figure2.png",
       device = "png",
       bg = "white",
       width = 180, 
       height = 120,
       units = "mm", 
       dpi = 300) 


## Figure 3 ####################################################################                        

# gross inconsistencies direction
xdata_sub <- xdata  |> 
  mutate(direction = ifelse(reported_p <= 0.05 & computed_p > 0.05, 
                                               "falsely reported significance", 
                                               ifelse(reported_p > 0.05 & computed_p <= 0.05,
                                               "falsely reported non-significance", "NA")
                            )) 

# overall prevalence of decision errors across years
xdata_sub_agg <- 
  xdata_sub |> 
  group_by(year) |> 
  mutate(checked_results = n(),
         significant = ifelse(reported_p < 0.05, TRUE, FALSE)) |> 
  group_by(year, direction) |> 
  reframe( 
    gross_errors = sum(decision_error),
    sign_results = sum(significant),
    prop_gross = (gross_errors / checked_results) *100)

# plot inconsistencies rates
ggplot(xdata_sub_agg |> filter(direction != "NA"),
       aes(x = year, 
           y = prop_gross,
           shape = direction)) + 
  geom_path(aes(group = direction, color = direction),
            linewidth = 2,
            lineend = "round") +
  # legend
  annotate("point", 
           x = 2015,
           y = 4.5,
           size = 5,
           color = "white",
           fill = "black",
           pch = 21) +
  annotate("point", 
           x = 2015,
           y = 4.0,
           size = 5,
           color = "white",
           fill = "#ee6002",
           pch = 21) +
  annotate("text", 
           x = 2016,
           y = 4.5,
           hjust = 0,
           color = "black",
           label = "falsely reported as significant") +
  annotate("text", 
           x = 2016,
           y = 4.0,
           hjust = 0,
           color = "black",
           label = "falsely reported as not significant") +
  scale_color_manual(values = c("#ee6002", "black")) +
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,0.5,1,2,3,4,5),
                     labels = c("0%", "0.5%", "1%", "2%", "3%", "4%", "5%")
  ) +
  scale_x_continuous(limits = c(2000,2024),
                     breaks =  c(2000, 2005, 2010, 2015, 2020, 2023)) +
  labs(x = "\nyear of publication",
       y = "percentage of decision inconsistencies\n") +
  theme_minimal() + 
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines"))
  
ggsave(filename = "../plots/figure3.png",
       device = "png",
       bg = "white",
       width = 160, 
       height = 96,
       units = "mm", 
       dpi = 300) 
 
