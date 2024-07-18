## title: Statistical reporting inconsistencies in Ling
## author: Timo B. Roettger
## contact: timo.b.roettger@gmail.com   
## date created: 2024-07-11
## date last edited:2024-07-11


## packages ####################################################################                        
 

# load in relevant libraries, install if not installed
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(rstudioapi, 
#                tidyverse, 
#                ggpubr, 
#                broom, 
#                statcheck,
#                janitor,
#                patchwork
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
# current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(current_working_dir)

## list all journal units
files <- list.files("../Journals", recursive = TRUE, pattern = "\\.pdf")

## how many articles?
length(files)
# 5804

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
#write.csv(stat, "../data/statcheck_data.csv", row.names = FALSE)


## data ########################################################################                        


## load in data
xdata <- read.csv("../data/statcheck_data.csv")

## split strings into year and journal, drop NAs
xdata <- xdata %>% 
  mutate(year = strtoi(substring(source, 1, 4)),
         journal = substring(source, 9, 11)) %>%
  drop_na(reported_p) %>%
  drop_na(computed_p) %>%
  filter(year != 2024)

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
                                      "assessible results", "inconsistencies", "decision inconsistencies")

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
  ggplot(per_article_long) +
  geom_bar(data = per_article_long,
           aes(x = journal, 
               y = 50,
               fill = type),
           stat = "identity")  +
  geom_bar(data = per_article_long,
           aes(x = journal, 
               y = 50),
           fill = "lightgrey",
           stat = "identity")  + 
  geom_bar(data = per_article_long %>% filter(type == "prop_has_error"),
           aes(x = journal, 
               y = proportion * 100,
               fill = type),
           fill = "#6002ee",
           stat = "identity") +  
  geom_text(data = per_article_long %>% filter(type == "prop_has_error"),
            aes(x = journal, 
                y = 30,
                label = paste0(round(proportion * 100, 0), "%")),
            color = "white") +
  geom_bar(data = per_article_long %>% filter(type == "prop_has_gross"),
           aes(x = journal, 
               y = proportion * 100),
           fill = "#ee6002",
           stat = "identity") +
  geom_text(data = per_article_long %>% filter(type == "prop_has_gross"),
            aes(x = journal, 
                y = 5,
                label = paste0(round(proportion * 100, 0), "%")),
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


## figure 2 ####################################################################                        


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
  facet_wrap(~ journal, ncol = 2) +
  labs(x = "\nyear of publication",
       y = "") +
   scale_y_continuous(breaks = c(0,.5,1),
                      labels = c("0%", "25%", "50%")
                      ) +
  scale_x_continuous(breaks =  c(2000, 2023)) +
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


## bias analysis ###############################################################                    

# calculate delta between reported and computed p
xdata <- xdata %>% 
  mutate(p_delta = (reported_p - computed_p))

xdata_sub <- xdata %>% 
  filter(error == T,
         p_comp == "=")

reportedVcomputed <- ggplot(xdata_sub) + 
  geom_point(data = xdata_sub %>% filter(decision_error == F),
             aes(x = reported_p, y = computed_p),
             alpha = 0.3,
             color = "#6002ee") +
  geom_point(data = xdata_sub %>% filter(decision_error == T),
             aes(x = reported_p, y = computed_p),
             alpha = 0.3,
             color = "#ee6002") +
  geom_smooth(data = xdata_sub,
              aes(x = reported_p, y = computed_p),
              method = "lm",
              se = F,
              color = "white",
              size = 1.5) +
  geom_smooth(data = xdata_sub,
              aes(x = reported_p, y = computed_p),
              method = "lm",
              se = F,
              color = "black",
              size = 1) +
  annotate("segment", 
           x = 0, xend = 1,
           y = 0.05, yend = 0.05,
           lty = "dashed") +
  annotate("segment", 
           x = 0.05, xend = 0.05,
           y = 0, yend = 1,
           lty = "dashed") +
  annotate("segment", 
           x = 0, xend = 1,
           y = 0.05, yend = 0.05,
           lty = "dashed") +
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.05, 0.5, 1)) + 
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.05, 0.5, 1)) +
  labs(x = "\nreported p value",
       y = "recalculated p value\n") +
  theme_minimal() 

ggsave(filename = "../plots/figure3.png",
       device = "png",
       bg = "white",
       width = 180, 
       height = 120,
       units = "mm", 
       dpi = 300) 

ggplot(xdata_sub) + 
  geom_point(data = xdata_sub %>% filter(decision_error == F),
             aes(x = reported_p, y = computed_p),
             alpha = 0.3,
             color = "#6002ee") +
  geom_point(data = xdata_sub %>% filter(decision_error == T),
             aes(x = reported_p, y = computed_p),
             alpha = 0.3,
             color = "#ee6002") +
  annotate("segment", 
           x = 0.05, xend = 0.05,
           y = 0, yend = 0.1,
           lty = "dashed") +
  annotate("segment", 
           x = 0, xend = 0.1,
           y = 0.05, yend = 0.05,
           lty = "dashed") +
  labs(x = "\nreported p value",
       y = "recalculated p value\n") +
  scale_x_continuous(limits = c(0,0.1),
                     breaks = c(0.001, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1)) + 
  scale_y_continuous(limits = c(0,0.1),
                     breaks = c(0.001, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1)) +
  theme_minimal() 


## liberal analysis ###############################################################                    
# 
# ## all errors
# xdata_errors <- xdata %>% 
#   summarise(errors = sum(error == T),
#             decision_errors = sum(decision_error == T))
# 
# # errors excluding p = 0
# xdata_errors_withou0 <- xdata %>% 
#   filter(!reported_p == 0 & p_comp %in% c("=", "<")) %>% 
#   summarise(errors = sum(error == T),
#             decision_errors = sum(decision_error == T))
# 
# # errors excluding p differences under e-03
# xdata_errors_nearenough <- xdata %>% 
#   mutate(close_enough = near(reported_p, computed_p, tol = 1e-2)) %>% 
#   filter(close_enough == F) %>% 
#   summarise(errors = sum(error == T),
#             decision_errors = sum(decision_error == T))
# 
# # errors excluding p = 0 AND p differences under e-03
# xdata_errors_withou0_nearenough <- xdata %>% 
#   mutate(close_enough = near(reported_p, computed_p, tol = 1e-2)) %>% 
#   filter(!reported_p == 0 & p_comp %in% c("=", "<"),
#          close_enough == F) %>% 
#   summarise(errors = sum(error == T),
#         decision_errors = sum(decision_error == T))
# 
# # bind to table
# error_type_tbl <- rbind(xdata_errors,
#                         xdata_errors_withou0,
#                         xdata_errors_nearenough,
#                         xdata_errors_withou0_nearenough)
# 
# ## wrangle to table
# error_type_tbl <- error_type_tbl %>% 
#   mutate(subset = c("all", "exclude p =/< 0", 
#                     "exclude differences <e-03", 
#                     "exclude p = 0 and differences <e-03"),
#          error_rate = errors/journal_summary_counts[9,4][[1]],
#          decision_error_rate = decision_errors/journal_summary_counts[9,4][[1]]) %>% 
#   select(subset, errors, decision_errors, error_rate, decision_error_rate)
# 
# ## recalculate proportion of checkable articles that contain 1 or more errors and 1 or more decision errors
# liberal_per_article_gross <- xdata %>% 
#   group_by(source) %>% 
#   summarise(checked_results = n()) 
# 
# liberal_per_article_errors <- xdata %>% 
#   group_by(source) %>% 
#   filter(!reported_p == 0 & p_comp %in% c("=", "<"),
#          close_enough == F) %>% 
#   summarise(errors = sum(error),
#             gross_errors = sum(decision_error),
#             has_error = ifelse(errors > 0, 1, 0),
#             has_gross = ifelse(gross_errors > 0, 1, 0)
#   ) %>% 
#   count(has_error)
# 
#   ungroup() %>% 
#   summarise(prop_has_error = mean(has_error),
#             prop_has_gross = mean(has_gross)
#   )
# 
# 
#   
# ## closer look at errors
# # evidence for sign switch
# xdata %>% 
#     filter(error == T,
#            p_comp == ">") %>% 
#     summarise(all = n(),
#               bias = sum(reported_p>computed_p))
# 
# # 56/57
# xdata %>% 
#   filter(error == T,
#          p_comp == "<") %>% 
#   summarise(all = n(),
#             bias = sum(reported_p>computed_p))
# 
# 
#     
#     
#   ggplot(aes(x=reported_p, y = computed_p)) +
#   geom_point(alpha = 0.3) +
#   scale_x_continuous(limits=c(0,0.1))
  
  
