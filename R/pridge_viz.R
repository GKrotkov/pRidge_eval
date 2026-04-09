library(scoutR)
library(tidyverse)
library(ggbeeswarm)

#######################
#### pRidge vs OPR ####
#######################

rm(list = ls())

load("data/pridge_vs_opr/pct_imp_combined.rda")

result <- result |>
    filter(year >= 2016)

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.6, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over OPR by year",
         subtitle = "By cross-validated MSE per event",
         x = "Year", y = "% Improvement")

ggsave("output/pridge_vs_opr_byyear.png", width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.6, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over OPR by competition week",
         subtitle = "By cross-validated MSE per event",
         x = "Week", y = "% Improvement")

ggsave("output/pridge_vs_opr_byweek.png", width = 6, height = 6, units = "in")

#######################
#### pRidge vs EPA ####
#######################

rm(list = ls())

load("data/pridge_vs_epa/pct_imp_combined.rda")

result <- result |>
    filter(year >= 2016)

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.6, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over EPA by year",
         subtitle = "By next-match-prediction MSE per event",
         x = "Year", y = "% Improvement")

ggsave("output/pridge_vs_epa_byyear.png", width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.6, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over EPA by competition week",
         subtitle = "By next-match-prediction MSE per event",
         x = "Week", y = "% Improvement")

ggsave("output/pridge_vs_epa_byweek.png", width = 6, height = 6, units = "in")
