library(scoutR)
library(tidyverse)

#################################
#### Combining yearwise data ####
#################################

#######################
#### pRidge vs OPR ####
#######################

rm(list = ls())

load("data/pridge_vs_opr/pct_imp_2016_to_2025.rda")

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6) +
    geom_violin(fill = "darkred", alpha = 0.8, width = 0.6) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "pRidge % improvement over OPR by year",
         subtitle = "By cross-validated MSE per event",
         x = "Year", y = "% Improvement")

ggsave("output/pridge_vs_opr_byyear.png", width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6) +
    geom_violin(fill = "darkred", alpha = 0.8, width = 0.6) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "pRidge % improvement over OPR by competition week",
         subtitle = "By cross-validated MSE per event",
         x = "Week", y = "% Improvement")

ggsave("output/pridge_vs_opr_byweek.png", width = 6, height = 6, units = "in")

#######################
#### pRidge vs EPA ####
#######################

rm(list = ls())

load("data/pridge_vs_epa/pct_imp_2016_to_2025.rda")

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6) +
    geom_violin(fill = "darkred", alpha = 0.8, width = 0.6) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    labs(title = "pRidge % improvement over EPA by year",
         subtitle = "By next-match-prediction MSE per event",
         x = "Year", y = "% Improvement")

ggsave("output/pridge_vs_epa_byyear.png", width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6) +
    geom_violin(fill = "darkred", alpha = 0.8, width = 0.6) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    labs(title = "pRidge % improvement over EPA by competition week",
         subtitle = "By next-match-prediction MSE per event",
         x = "Year", y = "% Improvement")

ggsave("output/pridge_vs_epa_byweek.png", width = 6, height = 6, units = "in")
