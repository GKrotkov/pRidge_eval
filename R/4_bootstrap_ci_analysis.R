# Computing bootstrap CIs for mean pct improvement on a per-year basis
rm(list = ls())
library(tidyverse)
library(gt)
library(glue)
library(parallel)

B <- 1000000
n_cores <- max(1, floor(detectCores() / 2))
blair_red <- "#a7000a"

######################
#### OPR Baseline ####
######################

load("data/pridge_vs_opr/pct_imp_2016_to_2026.rda")

boot_means <- replicate(B, {
    mean(sample(result$pct_imp, replace = TRUE), na.rm = TRUE)
})
overall_opr_bounds <- round(quantile(boot_means, c(0.025, 0.5, 0.975)), 2)

years <- unique(result$year)

start <- Sys.time()

cl <- makeCluster(n_cores)
clusterSetRNGStream(cl, iseed = 449)
clusterExport(cl, c("result", "B"))

opr_results <- parLapply(cl, years, function(yr) {
    year_df <- result[result$year == yr, ]
    boot_means <- replicate(B, {
        mean(sample(year_df$pct_imp, replace = TRUE), na.rm = TRUE)
    })
    quantile(boot_means, c(0.025, 0.5, 0.975))
})

stopCluster(cl)

opr_lower <- sapply(opr_results, `[`, 1)
opr_median <- sapply(opr_results, `[`, 2)
opr_upper <- sapply(opr_results, `[`, 3)

end <- Sys.time()
execution_time_opr <- end - start

######################
#### EPA Baseline ####
######################

load("data/pridge_vs_epa/pct_imp_2016_to_2026.rda")

boot_means <- replicate(B, {
    mean(sample(result$pct_imp, replace = TRUE), na.rm = TRUE)
})
overall_epa_bounds <- round(quantile(boot_means, c(0.025, 0.5, 0.975)), 2)

years <- unique(result$year)

start <- Sys.time()

cl <- makeCluster(n_cores)
clusterSetRNGStream(cl, iseed = 449)
clusterExport(cl, c("result", "B"))

epa_results <- parLapply(cl, years, function(yr) {
    year_df <- result[result$year == yr, ]
    boot_means <- replicate(B, {
        mean(sample(year_df$pct_imp, replace = TRUE), na.rm = TRUE)
    })
    quantile(boot_means, c(0.025, 0.5, 0.975))
})

stopCluster(cl)

epa_lower <- sapply(epa_results, `[`, 1)
epa_median <- sapply(epa_results, `[`, 2)
epa_upper <- sapply(epa_results, `[`, 3)

end <- Sys.time()
execution_time_epa <- end - start

#############
#### Viz ####
#############

n_events <- result |>
    group_by(year) |>
    summarize(count = n()) |>
    pull(count)

bounds <- tibble(
    `Year` = years,
    `# Events` = n_events,
    `OPR Baseline` = glue("({round(opr_lower, 1)}%, {round(opr_median, 1)}%, {round(opr_upper, 1)}%)"),
    `EPA Baseline` = glue("({round(epa_lower, 1)}%, {round(epa_median, 1)}%, {round(epa_upper, 1)}%)")
)

bounds_tab <- gt(bounds) |>
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

gtsave(bounds_tab, "output/bootstrap_cis.png")

save(overall_opr_bounds, overall_epa_bounds, bounds, opr_results, epa_results,
     file = "data/bootstrap_results.rda")

# Cochrane data from the 'rmeta'-package
base_data <- tibble::tibble(mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                            lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                            upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831),
                            study = c("Auckland", "Block", "Doran", "Gamsu",
                                      "Morrison", "Papageorgiou", "Tauesch"),
                            deaths_steroid = c("36", "1", "4", "14", "3", "1", "8"),
                            deaths_placebo = c("60", "5", "11", "20", "7", "7", "10"),
                            OR = c("0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02"))

base_data |>
    forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
               clip = c(0.1, 2.5),
               xlog = TRUE) |>
    fp_set_style(box = "royalblue",
                 line = "darkblue",
                 summary = "royalblue") |>
    fp_add_header(study = c("", "Study"),
                  deaths_steroid = c("Deaths", "(steroid)"),
                  deaths_placebo = c("Deaths", "(placebo)"),
                  OR = c("", "OR")) |>
    fp_append_row(mean  = 0.531,
                  lower = 0.386,
                  upper = 0.731,
                  study = "Summary",
                  OR = "0.53",
                  is.summary = TRUE) |>
    fp_set_zebra_style("#EFEFEF")
