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
    quantile(boot_means, c(0.025, 0.975))
})

stopCluster(cl)

opr_lower <- sapply(opr_results, `[`, 1)
opr_upper <- sapply(opr_results, `[`, 2)

end <- Sys.time()
execution_time_opr <- end - start

######################
#### EPA Baseline ####
######################

load("data/pridge_vs_epa/pct_imp_2016_to_2026.rda")
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
    quantile(boot_means, c(0.025, 0.975))
})

stopCluster(cl)

epa_lower <- sapply(epa_results, `[`, 1)
epa_upper <- sapply(epa_results, `[`, 2)

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
    `OPR Baseline` = glue("({round(opr_lower, 1)}, {round(opr_upper, 1)})"),
    `EPA Baseline` = glue("({round(epa_lower, 1)}, {round(epa_upper, 1)})")
)

gt(bounds) |>
    tab_header(title = "pRidge mean % improvement against baseline",
               subtitle = "Bootstrap 95% CIs, 1M replications") |>
    tab_style(
        style = cell_borders(
            sides = c("left", "right", "bottom", "top"),
            color = blair_red,
            weight = px(1)
        ),
        locations = cells_body(columns = 1:4)
    ) |>
    tab_style(
        style = cell_borders(
            sides = c("t", "b"),
            color = blair_red,
            weight = px(3)
        ),
        locations = list(cells_column_labels(), cells_stubhead(), cells_title())
    )
