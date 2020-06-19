

#' Calculate the Heteroskedasticity-Consistent Covariance Matrix Estimator.
#'
#' \code{white_hccme} is a helper function to calculate the HCCME
#'
#' @param lm_summ is an object of class "\code{lm}" that is the output of a
#' linear regression model.
#'
#' @return A data frame of the form of \code{coef(summary(lm_in))},
#' except with standard errors and the remaining statistics adjusted
#' for heteroskedasticity.
#'
white_hccme <- function(lm_in) {


  # Standard erros are the square root of thediagonal of the
  # White Heteroskedasticity-Corrected Covariance Matrix.
  White_se <- sqrt(diag(sandwich::vcovHC(lm_in)))

  # Recalculate the t-statistics and p-values.
  lm_stats_White <- coef(summary(lm_in))
  # Replace the SEs.
  lm_stats_White[, 'Std. Error'] <- White_se
  # Recalculate the t-stats.
  lm_stats_White[, 't value'] <- lm_stats_White[, 'Estimate'] / lm_stats_White[, 'Std. Error']
  # Recalculate the p-values.
  lm_stats_White[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_White[, 't value']),
                                       df = lm_in$df.residuals)

  return(lm_stats_White)
}




