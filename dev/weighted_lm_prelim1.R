
#--------------------------------------------------------------------------------
# Fixing weighted OLS
# So that weights account for sample size
# rather than (inverse) GLS weights.
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Generate an example of regression with aggregated data.
#--------------------------------------------------------------------------------


# Generate a nontrivial example.
ind_data <- data.frame(expand.grid(x1 = seq(1,3),
                                   x2 = seq(5,10),
                                   x3 = c(2, 2, 2, 4, 4, 6)))
summary(ind_data)

# Add an outcome according to a linear probability model.
prob_vec <- rowSums(ind_data[, c('x1', 'x2', 'x3')]) / 20


summary(prob_vec)

# Draw a binary dependent variable.
for (i in 1:nrow(ind_data)) {
  ind_data[i, 'y'] <- sample(c(0,1), size = 1,
                            replace = TRUE,
                            prob = c(1 - prob_vec[i], prob_vec[i]))
}
summary(ind_data)

# Aggregate the counts.
ind_data[, 'num'] <- 1

wtd_data <- aggregate(num ~ y + x1 + x2 + x3,
                      data = ind_data,
                      FUN = sum)

#--------------------------------------------------------------------------------
# Ok, now let's run some regressions.
#--------------------------------------------------------------------------------


ind_lm <- lm(y ~ x1 + x2 + x3, data = ind_data)


wtd_lm <- lm(y ~ x1 + x2 + x3, data = wtd_data, weights = num)


# Compare.
summary(ind_lm)
summary(wtd_lm)



# Compare the vcov() matrices.
vcov(wtd_lm)/vcov(ind_lm)
# Wrong. Just wrong.


#--------------------------------------------------------------------------------
# Need to make an adjustment.
#--------------------------------------------------------------------------------

# Copy the lm object.
adj_wtd_lm <- wtd_lm

# Replace the df for the residual (n - k).
adj_wtd_lm$df.residual <- sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm))

# Compare with the individual-level regression (the goal).
# summary(ind_lm)
# summary(adj_wtd_lm)
# THe RSE and df are now correct but the R-bar-squared is not.

# Make a copy of the summary to adjust the R-bar_squared.
sum_adj_wtd_lm <- summary(adj_wtd_lm)

# Look at the R-squared values.
# sum_adj_wtd_lm$adj.r.squared
# sum_adj_wtd_lm$r.squared

# Recalculate the R-bar-squared
1 - (1 - sum_adj_wtd_lm$r.squared) *
  (sum(wtd_data[, 'num']) - 1) /
  (sum(wtd_data[, 'num']) - length(coef(adj_wtd_lm)))

# Replace the value of the R-bar-squared.
sum_adj_wtd_lm$adj.r.squared <-
  1 - (1 - sum_adj_wtd_lm$r.squared) *
  (sum(adj_wtd_lm$weights) - 1) /
  (sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm)))

# Compare the adjusted weighted regression with
# the original individual-level data.
sum_adj_wtd_lm
summary(ind_lm)
# This is the answer!




#--------------------------------------------------------------------------------
# Define a function to adjust regression results
# for correct weighting with aggregated data.
#--------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------

# Test:
# Compare the adjusted weighted regression with
# the original individual-level data.
# summary(ind_lm)
# adj_wtd_lm_summary(wtd_lm)

#--------------------------------------------------------------------------------




#--------------------------------------------------------------------------------
# Try a fix from the internet.
# Close but not quite.
#--------------------------------------------------------------------------------


# Try the "fix" above.
fixed_wtd_lm <- wtd_lm
fixed_wtd_lm$df.residual <- with(fixed_wtd_lm, sqrt(sum(weights) - length(coefficients)))

summary(ind_lm)
summary(fixed_wtd_lm)
# Now they're too small.

# It just applies a fixed factor, not the correct correction.
vcov(wtd_lm)/vcov(fixed_wtd_lm)
vcov(fixed_wtd_lm)/vcov(wtd_lm)
vcov(fixed_wtd_lm)/vcov(ind_lm)

wtd_lm$df.residual



#--------------------------------------------------------------------------------
# This version worked.
# Adjust the df for the weighted observations.
# Then need to adjust the R-bar-squared on the summary
#--------------------------------------------------------------------------------


# Try another fix that operates on the df directly.
fixed_wtd_lm_2 <- wtd_lm
fixed_wtd_lm_2$df.residual <- sum(wtd_data[, 'num']) - length(coef(fixed_wtd_lm_2))

summary(fixed_wtd_lm_2)
# THe RSE and df are correct but the R-bar-squared is not.

sum_fixed_wtd_lm_2 <- summary(fixed_wtd_lm_2)

# Look at the R-squared values.
sum_fixed_wtd_lm_2$adj.r.squared
sum_fixed_wtd_lm_2$r.squared

# Recalculate the R-bar-squared
1 - (1 - sum_fixed_wtd_lm_2$r.squared) *
  (sum(wtd_data[, 'num']) - 1) /
  (sum(wtd_data[, 'num']) - length(coef(fixed_wtd_lm_2)))

# Replace the value of the R-bar-squared.
sum_fixed_wtd_lm_2$adj.r.squared <-
  1 - (1 - sum_fixed_wtd_lm_2$r.squared) *
  (sum(wtd_data[, 'num']) - 1) /
  (sum(wtd_data[, 'num']) - length(coef(fixed_wtd_lm_2)))


sum_fixed_wtd_lm_2
summary(ind_lm)


#--------------------------------------------------------------------------------
# Try a White SE adjustment
#--------------------------------------------------------------------------------


# Try using White corrected SEs.
library(sandwich)
White_ind_se <- sqrt(diag(vcovHC(ind_lm)))

# Not much of a change.
summary(ind_lm)
White_ind_se

# Apply the change to the weighted model.
White_wtd_se <- sqrt(diag(vcovHC(wtd_lm)))

# Not much change.
summary(wtd_lm)
White_wtd_se
# They're still too big.

# Try getting into the meat of the White sandwich.
meat_ind_se <- sqrt(diag(solve(meatHC(ind_lm,
                                      type = 'const', omega = NULL))))


#--------------------------------------------------------------------------------
# Calculate X'X from scratch
#--------------------------------------------------------------------------------


# Compare with X'X
# X <- matrix(ind_data[, c('x1', 'x2', 'x3')],
#             nrow = nrow(ind_data), ncol = 3)
X <- as.matrix(cbind(1, ind_data[, c('x1', 'x2', 'x3')]),
            nrow = nrow(ind_data), ncol = 3)
meat_ind_xtx_se <- sqrt(diag(solve(t(X) %*% X))) *
  sqrt(sum(ind_lm$residuals^2)/(104))


meat_ind_xtx_se
# Bingo. From 1st principles.


meat_ind_xtx_se/sqrt(diag(vcov(ind_lm)))
sqrt(diag(vcov(ind_lm)))/meat_ind_xtx_se


t(X) %*% X
meatHC(ind_lm, type = 'const')

meatHC(ind_lm, type = 'const')/(t(X) %*% X)
(t(X) %*% X)/meatHC(ind_lm, type = 'const')


sqrt(meatHC(ind_lm, type = 'const')/(t(X) %*% X))
sqrt((t(X) %*% X)/meatHC(ind_lm, type = 'const'))

(t(X) %*% X)/meatHC(ind_lm, type = 'const')/sqrt(104)
(t(X) %*% X)/meatHC(ind_lm, type = 'const')/104

#--------------------------------------------------------------------------------
# Compare the ratios to identify an adjustment.
#--------------------------------------------------------------------------------


meat_ind_se
sqrt(diag(vcov(ind_lm))) # Calculated SEs.

# Off by a constant.
meat_ind_se/sqrt(diag(vcov(ind_lm)))
sqrt(diag(vcov(ind_lm)))/meat_ind_se

sqrt(sum(ind_lm$residuals^2)/(104))
1/sqrt(sum(ind_lm$residuals^2)/(104))

sqrt(sum(ind_lm$residuals^2))/(104)
104/sqrt(sum(ind_lm$residuals^2))


# Apply to the weighted regression.
meat_wtd_se <- sqrt(diag(solve(meatHC(wtd_lm,
                                      omega = wtd_data[, 'num']))))


# Off by a constant.
meat_wtd_se/sqrt(diag(vcov(ind_lm)))
sqrt(diag(vcov(ind_lm)))/meat_wtd_se

# The usual formula for s_hat.
s_hat <- sqrt(sum(ind_lm$residuals^2)/(104))

s_hat_fix <- sqrt(sum((wtd_lm$residuals*wtd_data[, 'num'])^2)/(104))

# s_hat_wtd <- sqrt(sum(ind_lm$residuals^2)/(69))

s_hat_wtd <- sqrt(sum((wtd_lm$residuals)^2*wtd_data[, 'num'])/(69))

summary(wtd_lm)
summary(ind_lm)



sum((wtd_lm$residuals)^2*wtd_data[, 'num'])





meat_wtd_se/sqrt(diag(vcov(ind_lm))) / sum((wtd_lm$residuals)^2*wtd_data[, 'num'])



s_hat_fix^2
s_hat_wtd^2



s_hat_wtd/s_hat
s_hat/s_hat_wtd


sum(wtd_data[, 'num'])
nrow(wtd_data)

sum(wtd_data[, 'num'])/nrow(wtd_data)
nrow(wtd_data)/sum(wtd_data[, 'num'])



#--------------------------------------------------------------------------------
# Try a bunch of things.
#--------------------------------------------------------------------------------




(meat_wtd_se/sqrt(diag(vcov(ind_lm))))^2

(sqrt(diag(vcov(ind_lm)))/meat_wtd_se)^2


sqrt(meat_wtd_se/sqrt(diag(vcov(ind_lm))))
sqrt(sqrt(diag(vcov(ind_lm)))/meat_wtd_se)

s_hat <- sqrt(sum(ind_lm$residuals^2)/(104))


meat_wtd_se_adj <- meat_wtd_se*s_hat


meat_wtd_se_adj/sqrt(diag(vcov(ind_lm)))
sqrt(diag(vcov(ind_lm)))/meat_wtd_se_adj



#--------------------------------------------------------------------------------
# There's got to be another way.
# There is. See above. It is fixed.
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Example from internet that was close.
#--------------------------------------------------------------------------------



# Testing weights to make sure.
# summary(lm(c(8000, 50000, 116000) ~ c(6, 7, 8)))
# summary(lm(c(8000, 50000, 116000) ~ c(6, 7, 8), weight = c(123, 123, 246)))
# summary(lm(c(8000, 50000, 116000, 116000) ~ c(6, 7, 8, 8)))
# Not the same standard errors.
# The weights argument is to implement GLS,
# not to account for sampling weights.

correct_lm <- lm(c(8000, 50000, 116000, 116000) ~ c(6, 7, 8, 8))
incorrect_lm <- lm(c(8000, 50000, 116000) ~ c(6, 7, 8), weight = c(123, 123, 246))

summary(correct_lm)
summary(incorrect_lm)


# Apply the fix to the model.

fixed_lm <- incorrect_lm
fixed_lm$df.residual <- with(fixed_lm, sum(weights) - length(coefficients))

summary(fixed_lm)

# Recalculate standard errors.
# The old fashioned way.
vcov(incorrect_lm)
vcov(correct_lm)
# Compare the ratios.
vcov(incorrect_lm) / vcov(correct_lm)
# vcov matrix is twice the size.


# now compare with fix:
vcov(correct_lm)
vcov(fixed_lm)
# Not the same.
vcov(fixed_lm) / vcov(correct_lm)


(vcov(correct_lm) / vcov(fixed_lm))[1]
# = 245 = 246 weight, less 2 df for parameters.


# If variance is adjusted by a function of the weight,
# Then the se's should be adjusted by the quare root.


fixed_lm_2 <- incorrect_lm
fixed_lm_2$df.residual <- with(fixed_lm_2, sqrt(sum(weights) - length(coefficients)))

summary(fixed_lm_2)



# See what the adjustment does to the standard errors.
vcov(incorrect_lm) / vcov(fixed_lm)
vcov(incorrect_lm) / vcov(fixed_lm_2)

# Compare to the correct vcov()
vcov(correct_lm) / vcov(fixed_lm)
vcov(incorrect_lm) / vcov(correct_lm)
vcov(correct_lm) / vcov(fixed_lm_2)


# See how far off the original was.
vcov(incorrect_lm) / vcov(correct_lm)



# End
