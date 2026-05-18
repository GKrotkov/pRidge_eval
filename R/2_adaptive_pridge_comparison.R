rm(list = ls())
library(scoutR)
library(parallel)
library(doParallel)
library(dplyr)

# ── Helper functions ───────────────────────────────────────────────────────────

#' Load year data from file
#' @param year Integer year
#' @return year_data list or NULL if file doesn't exist
load_year_data <- function(year) {
    filename <- file.path(data_dir, paste0("year_", year, ".rda"))

    if (!file.exists(filename)) {
        stop("No data file found for year ", year,
             ". Run pull_data.R first.")
    }

    load(filename)  # Loads 'year_data' object
    return(year_data)
}

#' Fit Adaptive Prior Ridge
#'
#' Fits an adaptive prior ridge regression model using a single reweighting
#' step. A pilot pRidge fit (with LOOCV-selected lambda) is used to compute
#' per-coefficient penalty weights, which are then used in a second LOOCV-
#' selected pRidge fit.
#'
#' @param X (matrix) design matrix; columns are 1-hot encoded teams, each row
#'   an alliance-match
#' @param y (numeric vector) response vector; typically alliance scores
#' @param beta0 (numeric vector) prior means, named with TBA-legal team
#'   identifiers (i.e. "frc449"); must have length equal to ncol(X)
#' @param grid (numeric vector) grid of lambda values to search over via LOOCV
#' @param eps (numeric) stabilizing constant added to the denominator of the
#'   weight computation to avoid division by zero. Defaults to 1e-6.
#' @param n_cores (int or NULL) number of cores for parallel LOOCV. If NULL,
#'   defaults to max - 1.
#'
#' @return Named numeric vector of fitted coefficients, one per team column
#'   in X.
#'
#' @details
#' The adaptive penalty weights are computed as:
#'   w_j = 1 / (beta_hat_j^2 + eps)
#' where beta_hat_j is the j-th coefficient from the pilot fit. These weights
#' are passed as a diagonal penalty matrix to prior_ridge(), so each
#' coefficient is penalized in proportion to how small it appeared in the
#' pilot. A second LOOCV pass over `grid` selects a new global lambda for
#' the reweighted fit.
#'
#' @export
fit_adaptive_pridge <- function(X, y, beta0, grid, eps = 1e-6, n_cores = 1){
    X <- as.matrix(X)

    stopifnot(
        "X must be a matrix"                      = is.matrix(X),
        "y must have length equal to nrow(X)"     = length(y) == nrow(X),
        "beta0 must have length equal to ncol(X)" = length(beta0) == ncol(X)
    )

    # --- Step 1: Pilot fit ---
    # Select lambda* via LOOCV over the grid, then fit scalar pRidge
    pilot_mses   <- pridge_lambda_cv(X, y, beta0, grid,
                                     plot_mses = FALSE, n_cores = n_cores)
    lambda_pilot <- grid[which.min(pilot_mses)]
    beta_pilot   <- prior_ridge(X, y, lambda_pilot, beta0)

    # --- Step 2: Compute adaptive weights ---
    # w_j = 1 / (beta_hat_j^2 + eps)
    # Coefficients that are large in the pilot get a small penalty (less
    # shrinkage); near-zero coefficients get a large penalty (more shrinkage).
    weights <- 1 / (beta_pilot^2 + eps)

    # --- Step 3: Reweighted fit ---
    # Build a weighted penalty grid: for each scalar lambda in grid, the
    # effective per-coefficient penalty is lambda * w_j. We run LOOCV over
    # this weighted problem by passing the scaled weight vector to pridge_loocv.
    # prior_ridge() accepts a length-p lambda, which it places on the diagonal
    # of the penalty matrix.
    reweighted_mses <- sapply(grid, function(lambda) {
        scoutR:::pridge_loocv(X, y, lambda = lambda * weights, beta_0 = beta0)
    })

    lambda_reweighted <- grid[which.min(reweighted_mses)]

    # --- Step 4: Final fit ---
    beta_final <- prior_ridge(X, y,
                              lambda = lambda_reweighted * weights,
                              beta_0 = beta0)

    # Preserve team names from the design matrix columns
    names(beta_final) <- colnames(X)

    return(beta_final)
}

# ── CV helpers ─────────────────────────────────────────────────────────────────

coefs_mse <- function(design_test, response_test, coefs) {
    preds <- design_test %*% coefs
    mean((drop(preds) - response_test) ^ 2)
}

get_priors <- function(team_events_data) {
    priors <- sapply(team_events_data, function(te) te$epa$stats$start)
    names(priors) <- sapply(team_events_data, function(te) te$team)
    return(priors)
}

cv_fold <- function(fold, fold_ids, design, response, priors, grid) {
    train_idx <- fold_ids != fold
    test_idx  <- fold_ids == fold

    design_train   <- design[train_idx, ]
    design_test    <- design[test_idx,  ]
    response_train <- response[train_idx]
    response_test  <- response[test_idx]

    # ── Scalar pRidge ──────────────────────────────────────────────────────────
    scalar_mses   <- pridge_lambda_cv(
        design = design_train, response = response_train, priors = priors,
        grid = grid, plot_mses = FALSE, n_cores = 1
    )
    lambda_scalar <- grid[which.min(scalar_mses)]
    scalar_coefs  <- prior_ridge(design_train, response_train,
                                 lambda_scalar, priors)
    scalar_mse    <- coefs_mse(design_test, response_test, scalar_coefs)

    # ── Adaptive pRidge ────────────────────────────────────────────────────────
    adaptive_coefs <- fit_adaptive_pridge(
        X = design_train, y = response_train, beta0 = priors,
        grid = grid, n_cores = 1
    )
    adaptive_mse   <- coefs_mse(design_test, response_test, adaptive_coefs)

    list(
        scalar_mse    = scalar_mse,
        adaptive_mse  = adaptive_mse,
        lambda_scalar = lambda_scalar
    )
}

blank_result <- function() {
    data.frame(pct_imp = NA, scalar_mse = NA,
               adaptive_mse = NA, lambda_opt = NA)
}

#' Adaptive vs Scalar pRidge Comparison
#'
#' Runs 4-fold cross validation to compare the predictive performance of
#' adaptive vector lambda pRidge against scalar lambda pRidge for a single
#' event.
#'
#' @param event_key (chr) TBA-legal event key (e.g. "2025chcmp")
#' @param event_data_entry (list) cached event data entry with elements
#'   \code{matches} (qual match dataframe) and \code{team_events}
#'   (team_events_sb output for the event)
#' @param grid (numeric) grid of lambda values to search
#' @return single-row data.frame with columns:
#'   \item{pct_imp}{percent improvement of adaptive over scalar pRidge MSE}
#'   \item{scalar_mse}{4-fold CV MSE for scalar lambda pRidge}
#'   \item{adaptive_mse}{4-fold CV MSE for adaptive lambda pRidge}
#'   \item{lambda_opt}{mean scalar lambda_opt across folds}
pridge_adaptive_comparison <- function(
        event_key,
        event_data_entry,
        grid = exp(seq(log(0.01), log(20), length.out = 100))
) {
    matches     <- event_data_entry$matches
    team_events <- event_data_entry$team_events

    if (is.null(matches) || is.null(team_events)) return(blank_result())
    if (nrow(matches) < 4)                         return(blank_result())

    set.seed(449)

    priors <- get_priors(team_events)

    design   <- as.matrix(lineup_design_matrix(matches))
    response <- c(matches$blue_score, matches$red_score)

    # align priors to design matrix column order
    priors <- priors[match(colnames(design), scoutR:::tf(names(priors)))]

    k        <- 4
    fold_ids <- sample(rep(1:k, length.out = nrow(matches)))

    fold_results <- lapply(1:k, cv_fold,
                           fold_ids = fold_ids,
                           design   = design,
                           response = response,
                           priors   = priors,
                           grid     = grid)

    scalar_mse   <- mean(sapply(fold_results, function(x) x$scalar_mse))
    adaptive_mse <- mean(sapply(fold_results, function(x) x$adaptive_mse))
    lambda_opt   <- mean(sapply(fold_results, function(x) x$lambda_scalar))
    pct_imp      <- (scalar_mse - adaptive_mse) / scalar_mse * 100

    return(data.frame(pct_imp, scalar_mse, adaptive_mse, lambda_opt))
}

data_dir <- "data/raw/"

years <- setdiff(2016:2026, 2020:2021)
# HARDCODED FOR TESTING
years <- 2026

cat("=================================================\n")
cat("ADAPTIVE VS SCALAR LAMBDA COMPARISON (LOCAL DATA)\n")
cat("=================================================\n")
cat("Loading cached data from:", data_dir, "\n")
cat("Years to process:", paste(years, collapse = ", "), "\n")
cat("=================================================\n")

# Load all year data
all_year_data <- list()
for (year in years) {
    cat("Loading data for year", year, "...")
    year_data <- load_year_data(year)
    all_year_data[[as.character(year)]] <- year_data
    cat(" Done\n")
}

# Build event_keys list and event_data lookup
event_keys <- c()
event_data_lookup <- list()

for (year in years) {
    year_data <- all_year_data[[as.character(year)]]
    year_event_keys <- names(year_data$event_data)
    event_keys <- c(event_keys, year_event_keys)

    for (key in year_event_keys) {
        event_data_lookup[[key]] <- year_data$event_data[[key]]
    }
}

# Build qualifier_events metadata for final output
qualifier_events <- lapply(years, function(year) {
    all_year_data[[as.character(year)]]$events_metadata
}) |>
    dplyr::bind_rows()

cat("\nTotal events to process:", length(event_keys), "\n")
cat("Starting analysis...\n\n")

######################
#### Parallelized ####
######################

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

start <- Sys.time()

results_list <- foreach(
    key = event_keys,
    entry = lapply(event_keys, function(k) event_data_lookup[[k]]),
    .packages = c("scoutR"),
    .export = c(
        "pridge_adaptive_comparison",
        "cv_fold",
        "fit_adaptive_pridge",
        "coefs_mse",
        "get_priors",
        "blank_result"
    ),
    .errorhandling = "pass"
) %dopar% {
    tryCatch(
        {
            if (is.null(entry)) {
                stop("event_data_entry is NULL: key not found in event_data_lookup")
            }
            pridge_adaptive_comparison(key, entry)
        },
        error = function(e) {
            result <- blank_result()
            attr(result, "error_log") <- list(
                key       = key,
                message   = conditionMessage(e),
                class     = class(e),
                timestamp = Sys.time()
            )
            result
        }
    )
}

stopCluster(cl)

finish <- Sys.time()
execution_time <- finish - start

# ── Separate clean results from errored ones ───────────────────────────────────

error_log <- Filter(
    Negate(is.null),
    lapply(results_list, function(x) attr(x, "error_log"))
)

n_errors <- length(error_log)
n_ok     <- length(results_list) - n_errors

if (n_errors > 0) {
    cat(sprintf(
        "\nWARNING: %d / %d cases returned errors:\n",
        n_errors, length(results_list)
    ))
    for (entry in error_log) {
        cat(sprintf(
            "  [%s] key='%s'  msg='%s'\n",
            format(entry$timestamp, "%H:%M:%S"),
            entry$key,
            entry$message
        ))
    }
} else {
    cat("\nAll cases completed without errors.\n")
}

for (i in seq_along(results_list)) {
    attr(results_list[[i]], "error_log") <- NULL
}

result <- results_list |>
    bind_rows() |>
    mutate(key = event_keys) |>
    full_join(qualifier_events, by = "key") |>
    select(key, year, week, pct_imp, scalar_mse,
           lambda_opt, adaptive_mse, everything())

# Save results
output_dir <- "data/adaptive_vs_scalar/"
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

save(result, execution_time, n_cores, error_log,
     file = paste0(output_dir, "pct_imp_",
                   years[1], "_to_", tail(years, 1), ".rda"))

cat("\n=====================================\n")
cat("COMPLETE\n")
cat("=====================================\n")
cat("Execution time:", round(execution_time, 2),
    attr(execution_time, "units"), "\n")
cat("Events processed:", nrow(result), "\n")
cat("Events with valid results:", sum(!is.na(result$pct_imp)), "\n")
cat("Results saved to:", output_dir, "\n")
cat("=====================================\n")
