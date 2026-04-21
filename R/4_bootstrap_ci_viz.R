# Visualizing bootstrap CIs
rm(list = ls())

library(tidyverse)
library(gt)
library(forestplot)

blair_red <- "#a7000a"
load("data/bootstrap_results.rda")

###############
#### Table ####
###############

bounds_gt <- gt(bounds_tbl) |>
    tab_header(title = "pRidge Mean % Improvement against Baseline",
               subtitle = "Bootstrap 95% CIs (2.5th, 50th, 97.5th)") |>
    tab_style(
        style = cell_borders(
            sides = c("left", "right", "bottom", "top"),
            color = "black",
            weight = px(2)
        ),
        locations = cells_body(columns = 1:4)
    ) |>
    tab_style(
        style = cell_borders(
            sides = c("t"),
            color = blair_red,
            weight = px(4)
        ),
        locations = list(cells_title())
    ) |>
    tab_style(
        style = cell_borders(
            sides = c("t", "b", "l", "r"),
            color = "black",
            weight = px(3)
        ),
        locations = list(cells_column_labels())
    )

gtsave(bounds_gt, "output/bootstrap_cis.png")

#####################
#### Forest Plot ####
#####################

bounds_epa$baseline <- "EPA"
bounds_opr$baseline <- "OPR"

viz <- rbind(bounds_opr, bounds_epa) |>
    rename(labeltext = year, mean = median) |>
    mutate(lower = round(lower, digits = 1),
           mean = round(mean, digits = 1),
           upper = round(upper, digits = 1))

viz |>
    group_by(baseline) |>
    forestplot()
