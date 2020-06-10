
context("Linear Probability Model")


test_that("Linear Probability Model results are correct", {

  # Generate data.
  set.seed(42)
  ind_lpm_data <- gen_agg_lm(model = 'lpm')
  ind_lpm_data[, 'num'] <- 1
  agg_lpm_data <- aggregate(num ~ y + x1 + x2 + x3,
                            data = ind_lpm_data,
                            FUN = sum)

  # Estimate models.
  ind_lpm_lm <- lm(y ~ x1 + x2 + x3, data = ind_lpm_data)
  agg_lpm_lm <- agg_lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num)

  # Compare results.
  expect_equal(ind_lpm_lm$coefficients, agg_lpm_lm$coefficients)
  expect_equal(ind_lpm_lm$rank, agg_lpm_lm$rank)
  expect_equal(ind_lpm_lm$df.residual, agg_lpm_lm$df.residual)
  expect_equal(ind_lpm_lm$coefficients, agg_lpm_lm$coefficients)
  expect_equal(ind_lpm_lm$coefficients, agg_lpm_lm$coefficients)
  expect_equal(ind_lpm_lm$coefficients, agg_lpm_lm$coefficients)
})


test_that("Summary of Linear Probability Model is correct", {

  # Generate data.
  set.seed(42)
  ind_lpm_data <- gen_agg_lm(model = 'lpm')
  ind_lpm_data[, 'num'] <- 1
  agg_lpm_data <- aggregate(num ~ y + x1 + x2 + x3,
                            data = ind_lpm_data,
                            FUN = sum)

  # Estimate models.
  ind_lpm_lm <- lm(y ~ x1 + x2 + x3, data = ind_lpm_data)
  agg_lpm_lm <- agg_lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num)

  # Capture summary output.
  ind_lpm_lm_summ <- capture.output(summary(ind_lpm_lm))
  agg_lpm_lm_summ <- capture.output(summary_agg_lm(agg_lpm_lm))

  # Compare results.
  expect_equal(ind_lpm_lm_summ[9:21], agg_lpm_lm_summ[9:21])
})
