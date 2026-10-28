# EDA on the chosen lambda values for prior ridge
rm(list = ls())
library(tidyverse)
source("R/utils.R")

# Configuration
score_type <- "no_foul_score"   # "score" for penalized scores
stopifnot(score_type %in% c("score", "no_foul_score"))

load(results_path("opr", score_type, 2016:2026))
vsopr <- result |>
    select(key, year, week, pct_imp_opr = pct_imp, pridge_mse_opr = pridge_mse,
           lambda_opt_opr = lambda_opt, opr_mse) |>
    scoutR:::round_numerics()

load(results_path("epa", score_type, 2016:2026))
vsepa <- result |>
    select(key, year, week, pct_imp_epa = pct_imp, pridge_mse_epa = pridge_mse,
           epa_mse, lambda_mean_epa = lambda_mean,
           lambda_median_epa = lambda_median, lambda_sd_epa = lambda_sd,
           lambda_max_epa = lambda_max, lambda_min_epa = lambda_min) |>
    scoutR:::round_numerics()

result <- merge(vsopr, vsepa)

rm(list = setdiff(ls(), "result"))

viz <- result |>
    select(key, year, week, starts_with("lambda")) |>
    pivot_longer(
        cols = starts_with("lambda"), names_to = "type", values_to = "lambda"
    ) |>
    mutate(type = substr(type, 8, nchar(type)))

ggplot(viz, aes(x = lambda, color = type)) +
    geom_density() +
    theme_bw()

viz <- viz |>
    filter(type %in% c("max_epa", "min_epa", "sd_epa", "opt_opr"))

ggplot(viz, aes(x = lambda, color = type)) +
    geom_density() +
    theme_bw()

