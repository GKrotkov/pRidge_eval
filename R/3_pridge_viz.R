library(scoutR)
library(tidyverse)
library(ggbeeswarm)
library(gt)

rm(list = ls())
source("R/utils.R")

# Configuration
score_type <- "no_foul_score"   # "score" for penalized scores
stopifnot(score_type %in% c("score", "no_foul_score"))
score_label <- if (score_type == "score") "official score" else "no-foul score"

#######################
#### pRidge vs OPR ####
#######################

load(results_path("opr", score_type, 2016:2026))

result <- result |>
    filter(year >= 2016)

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.4, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over OPR by year",
         subtitle = paste0("By cross-validated MSE per event (", score_label, ")"),
         x = "Year", y = "% Improvement")

ggsave(paste0("output/pridge_vs_opr_byyear_", score_type, ".png"),
       width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.4, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over OPR by competition week",
         subtitle = paste0("By cross-validated MSE per event (", score_label, ")"),
         x = "Week", y = "% Improvement")

ggsave(paste0("output/pridge_vs_opr_byweek_", score_type, ".png"),
       width = 6, height = 6, units = "in")

#######################
#### pRidge vs EPA ####
#######################

load(results_path("epa", score_type, 2016:2026))

result <- result |>
    filter(year >= 2016)

ggplot(result, aes(x = as.character(year), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.4, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over EPA by year",
         subtitle = paste0("By next-match-prediction MSE per event (",
                           score_label, ")"),
         x = "Year", y = "% Improvement")

ggsave(paste0("output/pridge_vs_epa_byyear_", score_type, ".png"),
       width = 6, height = 6, units = "in")

ggplot(result, aes(x = as.character(week + 1), y = (pct_imp / 100))) +
    geom_boxplot(width = 0.6, alpha = 0.8, fill = "darkred") +
    geom_quasirandom(alpha = 0.4, shape = 21, size = 1, width = 0.4) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(breaks = seq(-0.6, 0.7, by = 0.2),
                       labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(title = "pRidge % improvement over EPA by competition week",
         subtitle = paste0("By next-match-prediction MSE per event (",
                           score_label, ")"),
         x = "Week", y = "% Improvement")

ggsave(paste0("output/pridge_vs_epa_byweek_", score_type, ".png"),
       width = 6, height = 6, units = "in")

#########################
#### pRidge accuracy ####
#########################

load(results_path("opr", score_type, 2016:2026))

opr_acc <- result |>
    group_by(year) |>
    summarize(
        n = n(),
        `OPR` = mean(opr_acc, na.rm = TRUE),
        `pRidge` = mean(pridge_acc, na.rm = TRUE)
    ) |>
    scoutR:::round_numerics(digits = 4)

load(results_path("epa", score_type, 2016:2026))

stitch <- result |>
    group_by(year) |>
    summarize(
        `EPA` = mean(epa_acc, na.rm = TRUE)
    ) |>
    scoutR:::round_numerics(digits = 4)

viz <- merge(opr_acc, stitch, by = "year") |>
    select(Year = year, `# Events` = n, `OPR`, `EPA`, `pRidge`)

acc_cols <- c("OPR", "EPA", "pRidge")

# Problem currently: this isn't comparison apples to apples; EPA is
# next-match-prediction while OPR and pRidge are cross validated
# Pre-compute weighted means
w <- viz$`# Events`

weighted_means <- list(
    Overall ~ sum(. * w) / sum(w)
)

gt(viz) |>
    tab_header(title = "pRidge accuracy comparisons",
               subtitle = paste0("Response: ", score_label)) |>
    fmt_percent(columns = 3:5) |>
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
            columns = 3,
            rows = apply(viz[acc_cols], 1, which.max) == 1
        )
    ) |>
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
            columns = 4,
            rows = apply(viz[acc_cols], 1, which.max) == 2
        )
    ) |>
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
            columns = 5,
            rows = apply(viz[acc_cols], 1, which.max) == 3
        )
    ) |>
    grand_summary_rows(
        columns = 3:5,
        fns = weighted_means,
        ~ fmt_percent(.)
    )
