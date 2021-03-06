

#' Calculate the Heteroskedasticity-Consistent Covariance Matrix Estimator.
#'
#' \code{white_hccme} is a helper function to calculate the HCCME,
#' to test the calculation for un-aggregated data, i.e. data
#' with weights either \code{NULL} or equal to 1.
#'
#' @param lm_in is an object of class "\code{lm}" that is the output of a
#' linear regression model.
#'
#' @return A data frame of the form of \code{coef(summary(lm_in))},
#' except with standard errors and the remaining statistics adjusted
#' for heteroskedasticity.
#'
#' @seealso The \code{vcovHC} function in the \code{sandwich} package.
#'
white_hccme <- function(lm_in) {


  # Standard erros are the square root of the diagonal of the
  # White Heteroskedasticity-Corrected Covariance Matrix.
  # Note that type = 'HC' calculates White's estimator.
  vcov_mat <- sandwich::vcovHC(lm_in, type = 'HC')
  White_se <- sqrt(diag(vcov_mat))

  # Recalculate the t-statistics and p-values.
  lm_stats_White <- coef(summary(lm_in))
  # Replace the SEs.
  lm_stats_White[, 'Std. Error'] <- White_se
  # Recalculate the t-stats.
  lm_stats_White[, 't value'] <- lm_stats_White[, 'Estimate'] / lm_stats_White[, 'Std. Error']
  # Recalculate the p-values.
  lm_stats_White[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_White[, 't value']),
                                       df = lm_in$df.residual)

  lm_out <- list(vcov_hccme = vcov_mat,
                 coef_hccme = lm_stats_White)

  return(lm_out)
}



#' Calculate the Heteroskedasticity-Consistent Covariance Matrix Estimator slowly.
#'
#' \code{white_hccme_slow} is a helper function to calculate the HCCME,
#' to test the calculation for comparison with un-aggregated data, i.e. data
#' with weights either \code{NULL} or equal to 1.
#' It calculates the covariance matrix from first principles, i.e. with loops.
#'
#' @param lm_in is an object of class "\code{lm}" that is the output of a
#' linear regression model.
#'
#' @return A list containing two elements. The first is the HCCME covariance
#' matrix. the second is a data frame of the form of \code{coef(summary(lm_in))},
#' except with standard errors and the remaining statistics adjusted
#' for heteroskedasticity.
#'
#' @seealso \code{white_hccme}, which uses \code{vcovHC} function in the
#' \code{sandwich} package.
#'
white_hccme_slow <- function(lm_in) {

  # Calculate the meat of the sandwich matrix.
  if (length(lm_in$x) == 0) {
    stop('Missing the model matrix. ',
         'Set x = TRUE when calling the lm function.')
  }
  n <- nrow(lm_in$x)
  k <- ncol(lm_in$x)
  vcov_meat <- matrix(0, nrow = k, ncol = k)
  if (!is.null(lm_in$weights)) {
    wtd_res_sq <- lm_in$weights * lm_in$residuals^2
    vcov_dough <- matrix(rep(sqrt(lm_in$weights), k),
                         nrow = n, ncol = k) * lm_in$x
    vcov_bread <- solve(t(vcov_dough) %*% vcov_dough)
  } else {
    wtd_res_sq <- lm_in$residuals^2
    vcov_bread <- solve(t(lm_in$x) %*% lm_in$x)
  }
  for (i in 1:n) {
    vcov_meat <- vcov_meat + wtd_res_sq[i] *
      t(lm_in$x[i, , drop = FALSE]) %*% lm_in$x[i, ]
  }
  vcov_mat <- vcov_bread %*% vcov_meat %*% vcov_bread

  # Standard erros are the square root of the diagonal of the
  # White Heteroskedasticity-Corrected Covariance Matrix.
  White_se <- sqrt(diag(vcov_mat))


  # Recalculate the t-statistics and p-values.

  # First obtain the regression output.
  if (class(lm_in) == 'agg_lm') {
    lm_stats_White <- coef(summary_agg_lm(lm_in))
  } else {
    lm_stats_White <- coef(summary(lm_in))
  }


  # Replace the SEs.
  lm_stats_White[, 'Std. Error'] <- White_se
  # Recalculate the t-stats.
  lm_stats_White[, 't value'] <- lm_stats_White[, 'Estimate'] / lm_stats_White[, 'Std. Error']
  # Recalculate the p-values.
  lm_stats_White[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_White[, 't value']),
                                       df = lm_in$df.residual)

  lm_out <- list(vcov_hccme = vcov_mat,
                 coef_hccme = lm_stats_White)

  return(lm_out)
}


#' Calculate the Heteroskedasticity-Consistent Covariance Matrix Estimator less slowly.
#'
#' \code{white_hccme_med} calculates the HCCME for a linear regression model,
#' to match the calculation for un-aggregated data, i.e. data
#' with weights either \code{NULL} or equal to 1.
#' It calculates the covariance matrix from first principles, i.e. with loops,
#' but with loops in the rows and columns of the covariance matrix, not in the sample size.
#'
#' @param lm_in is an object of class "\code{lm}" that is the output of a
#' linear regression model.
#'
#' @return A list containing two elements. The first is the HCCME covariance
#' matrix. the second is a data frame of the form of \code{coef(summary(lm_in))},
#' except with standard errors and the remaining statistics adjusted
#' for heteroskedasticity.
#'
#' @seealso \code{white_hccme}, which uses \code{vcovHC} function in the
#' \code{sandwich} package.
#'
white_hccme_med <- function(lm_in) {

  # Calculate the meat of the sandwich matrix.
  if (length(lm_in$x) == 0) {
    stop('Missing the model matrix. ',
         'Set x = TRUE when calling the lm function.')
  }
  n <- nrow(lm_in$x)
  k <- ncol(lm_in$x)
  vcov_meat <- matrix(0, nrow = k, ncol = k)
  if (!is.null(lm_in$weights)) {
    wtd_res_sq <- lm_in$weights * lm_in$residuals^2
    vcov_dough <- matrix(rep(sqrt(lm_in$weights), k),
                         nrow = n, ncol = k) * lm_in$x
    vcov_bread <- solve(t(vcov_dough) %*% vcov_dough)
  } else {
    wtd_res_sq <- lm_in$residuals^2
    vcov_bread <- solve(t(lm_in$x) %*% lm_in$x)
  }
  for (i in 1:k) {
    for (j in 1:i) {
      vcov_meat[i, j] <- sum(lm_in$x[, i] * lm_in$x[, j] * wtd_res_sq)
      vcov_meat[j, i] <- vcov_meat[i, j]
    }
    # vcov_meat <- vcov_meat + wtd_res_sq[i] *
    #   t(lm_in$x[i, , drop = FALSE]) %*% lm_in$x[i, ]
  }
  vcov_mat <- vcov_bread %*% vcov_meat %*% vcov_bread

  # Standard erros are the square root of the diagonal of the
  # White Heteroskedasticity-Corrected Covariance Matrix.
  White_se <- sqrt(diag(vcov_mat))


  # Recalculate the t-statistics and p-values.

  # First obtain the regression output.
  if (class(lm_in) == 'agg_lm') {
    lm_stats_White <- coef(summary_agg_lm(lm_in))
  } else {
    lm_stats_White <- coef(summary(lm_in))
  }


  # Replace the SEs.
  lm_stats_White[, 'Std. Error'] <- White_se
  # Recalculate the t-stats.
  lm_stats_White[, 't value'] <- lm_stats_White[, 'Estimate'] / lm_stats_White[, 'Std. Error']
  # Recalculate the p-values.
  lm_stats_White[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_White[, 't value']),
                                       df = lm_in$df.residual)

  lm_out <- list(vcov_hccme = vcov_mat,
                 coef_hccme = lm_stats_White)

  return(lm_out)
}



