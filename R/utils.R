# Add no-foul alliance scores next to the official ones
add_no_foul_scores <- function(matches){
    if (is.null(matches) ||
        !all(c("red_foulPoints", "blue_foulPoints") %in% names(matches))) {
        return(matches)
    }
    matches$red_no_foul_score  <- matches$red_score  - matches$red_foulPoints
    matches$blue_no_foul_score <- matches$blue_score - matches$blue_foulPoints
    return(matches)
}

# Stack an alliance-level score blue-then-red, matching lineup_design_matrix().
# score_type follows scoutR's "(red|blue)_<response>" convention:
# "score" (unpenalized) or "no_foul_score".
stacked_response <- function(matches, score_type){
    cols <- paste0(c("blue_", "red_"), score_type)
    missing <- setdiff(cols, names(matches))
    if (length(missing) > 0){
        stop("matches is missing: ", paste(missing, collapse = ", "))
    }
    c(matches[[cols[1]]], matches[[cols[2]]])
}
