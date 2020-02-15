
#' Create a revised summary of an lm object.
#'
#' @param wtd_lm an lm object from a regression with frequencies as the weights.
#' @return The revised summary of the lm object \code{wtd_lm}.
#'
#'
adj_wtd_lm_summary <- function(wtd_lm) {

  # Copy the lm object.
  adj_wtd_lm <- wtd_lm

  # Replace the df for the residual (n - k).
  adj_wtd_lm$df.residual <- sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm))

  # Make a copy of the summary to adjust the R-bar_squared.
  sum_adj_wtd_lm <- summary(adj_wtd_lm)

  # Replace the value of the R-bar-squared.
  sum_adj_wtd_lm$adj.r.squared <-
    1 - (1 - sum_adj_wtd_lm$r.squared) *
    (sum(adj_wtd_lm$weights) - 1) /
    (sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm)))

  # Return the summary with the adjusted weighted regression
  # to replace the original individual-level data.
  return(sum_adj_wtd_lm)

}
