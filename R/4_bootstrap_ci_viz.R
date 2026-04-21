# Visualizing bootstrap CIs
rm(list = ls())

library(tidyverse)
library(gt)
library(forestplot)
library(forestploter)

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
    forestplot(ci.vertices = TRUE,
               ci.vertices.height = 0.1,
               boxsize = 0.2,
               # cutting out the high outlier 2018 performance for visibility
               clip = c(-5, 25),
               xlab = "% improvement over baseline") |>
    fp_add_lines("steelblue") |>
    fp_add_header("Subgroup") |>
    fp_set_style(box = c("blue", blair_red),
                 default = gpar(verticies = TRUE)) |>
    fp_set_zebra_style("lightpink") |>
    fp_set_favors(low = "Favors baseline", high = "Favors pRidge",
                  txt_gp = gpar(cex = 1))

# forestploter version
viz <- rbind(bounds_opr, bounds_epa) |>
    rename(Year = year, `# of Events` = n_events) |>
    mutate(lower = round(lower, digits = 1),
           median = round(median, digits = 1),
           upper = round(upper, digits = 1),
           ci = paste0(median, " (", lower, ", ", upper, ")"))

# Prepare the data for plotting
viz_plot <- viz %>%
    mutate(
        # Rename columns for display
        Year = labeltext,
        `# of Events` = as.character(n_events),
        # Add a blank column for the forest plot
        ` ` = paste(rep(" ", 20), collapse = " "),
        # Create grouping variable for colors
        group = baseline
    )

# Define colors for each baseline group
viz_plot <- viz_plot %>%
    mutate(
        color = case_when(
            baseline == "OPR" ~ "#0072B2",  # Blue for OPR
            baseline == "EPA" ~ "#D55E00"   # Orange for EPA
        )
    )

# Create the forest plot
p <- forest(
    viz_plot[, c("Year", "# of Events", " ")],
    est = viz_plot$median,
    lower = viz_plot$lower,
    upper = viz_plot$upper,
    sizes = 0.4,
    ci_column = 3,
    ref_line = 0,
    arrow_lab = c("Favors Left", "Favors Right"),
    xlim = NULL,  # Auto-scale
    theme = forest_theme(
        core = list(fg_params = list(hjust = 0, x = 0.01)),
        colhead = list(fg_params = list(fontface = "bold"))
    )
)

# Add colors to the confidence intervals
p <- edit_plot(
    p,
    row = 1:nrow(viz_plot),
    gp = gpar(col = viz_plot$color, fill = viz_plot$color)
)

# Display the plot
plot(p)

