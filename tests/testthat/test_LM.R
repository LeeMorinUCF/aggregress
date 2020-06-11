
context("Linear Regression Model")


test_that("Linear Regression Model results are correct", {

  # Generate data.
  set.seed(42)
  ind_lm_data <- gen_agg_lm(model = 'lm')
  # Aggregate the counts.
  ind_lm_data[, 'num'] <- 1
  # Add the squared responses for calculation of statistics.
  ind_lm_data[, 'y_squared'] <- ind_lm_data[, 'y']^2
  agg_lm_data <- aggregate(cbind(num, y, y_squared) ~ x1 + x2 + x3,
                           data = ind_lm_data,
                           FUN = sum)
  agg_lm_data[, 'y_bar'] <- agg_lm_data[, 'y']/agg_lm_data[, 'num']


  # Estimate models.
  ind_lm_reg <- lm(y ~ x1 + x2 + x3, data = ind_lm_data)
  agg_lm_reg <- agg_lm(y_bar ~ x1 + x2 + x3, data = agg_lm_data, weights = num,
                       y_squared = agg_lm_data[, 'y_squared'])

  # Compare results.
  expect_equal(ind_lm_reg$coefficients, agg_lm_reg$coefficients)
  expect_equal(ind_lm_reg$rank, agg_lm_reg$rank)
  expect_equal(ind_lm_reg$df.residual, agg_lm_reg$df.residual)
})


test_that("Summary of Linear Regression Model is correct", {

  # Generate data.
  set.seed(42)
  ind_lm_data <- gen_agg_lm(model = 'lm')
  # Aggregate the counts.
  ind_lm_data[, 'num'] <- 1
  # Add the squared responses for calculation of statistics.
  ind_lm_data[, 'y_squared'] <- ind_lm_data[, 'y']^2
  agg_lm_data <- aggregate(cbind(num, y, y_squared) ~ x1 + x2 + x3,
                           data = ind_lm_data,
                           FUN = sum)
  agg_lm_data[, 'y_bar'] <- agg_lm_data[, 'y']/agg_lm_data[, 'num']


  # Estimate models.
  ind_lm_reg <- lm(y ~ x1 + x2 + x3, data = ind_lm_data)
  agg_lm_reg <- agg_lm(y_bar ~ x1 + x2 + x3, data = agg_lm_data, weights = num,
                       y_squared = agg_lm_data[, 'y_squared'])

  # Capture summary output.
  ind_lm_reg_summ <- capture.output(summary(ind_lm_reg))
  agg_lm_reg_summ <- capture.output(summary_agg_lm(agg_lm_reg))

  # Compare results.
  expect_equal(ind_lm_reg_summ[9:19], agg_lm_reg_summ[10:20])
})
