##################################################
#
# Agregress Testing and Demonstration
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# June 10, 2020
#
##################################################
#
# aggregress_demo demonstrates a comparison of analysis
#   using aggregated and otherwise identical
#   pre-aggregated data.
#
# Dependencies:
#   None, besides the aggregress package.
#
# This version compares all output from the lm
# function and the agg_lm function with a LPM.
#
##################################################




##################################################
# Linear Probability Models
# (Binary Dependent Variable)
##################################################

#--------------------------------------------------
# Generate sample data
#--------------------------------------------------

set.seed(42)

# Generate an example.
ind_lpm_data <- data.frame(expand.grid(x1 = seq(1,3),
                                   x2 = seq(5,10),
                                   x3 = c(2, 2, 2, 4, 4, 6)))

# Add an outcome according to a linear probability model.
# All coefficients are ones.
# prob_vec <- rowSums(ind_lpm_data[, c('x1', 'x2', 'x3')]) / 20
ind_lpm_data[, 'probs'] <- rowSums(ind_lpm_data[, c('x1', 'x2', 'x3')]) / 20


# Draw a binary dependent variable.
ind_lpm_data[, 'y'] <- as.integer(runif(nrow(ind_lpm_data)) <=  ind_lpm_data[, 'probs'])

# Variables appear correct.
# All inside the unit interval.
summary(ind_lpm_data)


# Aggregate the counts.
ind_lpm_data[, 'num'] <- 1

agg_lpm_data <- aggregate(num ~ y + x1 + x2 + x3,
                      data = ind_lpm_data,
                      FUN = sum)

# Test that this is an LPM.
all(agg_lpm_data[, 'y'] %in% c(0, 1))



#--------------------------------------------------
# Estimate Linear Model from Individual Data
#--------------------------------------------------

ind_lpm_lm <- lm(y ~ x1 + x2 + x3, data = ind_lpm_data)




#--------------------------------------------------
# Estimate Linear Model from Aggregated Data
#--------------------------------------------------


wtd_lpm_lm <- lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num)

# Preliminary version.
agg_lpm_lm_summ <- adj_wtd_lm_summary(wtd_lpm_lm)

# Modified lm function.
agg_lpm_lm <- agg_lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num)

summary_agg_lm(agg_lpm_lm)


#--------------------------------------------------
# Compare the results
#--------------------------------------------------


# Compare summaries.
summary(ind_lpm_lm)
# summary(agg_lpm_lm)
summary_agg_lm(agg_lpm_lm)



# Goal is to make sure that the lm objects are the same.

attributes(ind_lpm_lm)
# $`names`
# [1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values"
# [6] "assign"        "qr"            "df.residual"   "xlevels"       "call"
# [11] "terms"         "model"

# Compare each attribute with the weighted counterpart.


# [1,] "coefficients"
ind_lpm_lm$coefficients
agg_lpm_lm$coefficients
# Same.

# [2,] "residuals"
summary(ind_lpm_lm$residuals)
summary(agg_lpm_lm$residuals)
# Not the same but ok.
summary(agg_lpm_lm$residuals*agg_lpm_lm$weights)
# Weighted residuals have mean zero.

# [3,] "effects"
ind_lpm_lm$effects
agg_lpm_lm$effects
# Not sure what this is.

# [4,] "rank"
ind_lpm_lm$rank
agg_lpm_lm$rank
# Same.


# [5,] "fitted.values"
summary(ind_lpm_lm$fitted.values)
summary(agg_lpm_lm$fitted.values)
# Not the same length but similar.

# [6,] "assign"
ind_lpm_lm$assign
agg_lpm_lm$assign
# Same.


# [7,] "qr"
ind_lpm_lm$qr
agg_lpm_lm$qr
# Not the same but are the length of the data frame.
# Matrices in the qr decomposition?


# [8,] "df.residual"
ind_lpm_lm$df.residual
agg_lpm_lm$df.residual
# Same.

# [9,] "xlevels"
ind_lpm_lm$xlevels
agg_lpm_lm$xlevels
# Empty list in each case.

# [10,] "call"
ind_lpm_lm$call
agg_lpm_lm$call
# Different but accurate.


# [11,] "terms"
ind_lpm_lm$terms
agg_lpm_lm$terms
# Only difference is the extra column of weights.

# [12,] "model"
ind_lpm_lm$model
agg_lpm_lm$model
# Different but match true number of observations.


# What matters is the coefficient table and summary.
ind_lpm_coef <- coef(ind_lpm_lm)
agg_lpm_coef <- coef(agg_lpm_lm)
# Not much to check here, given the above.
ind_lpm_coef
agg_lpm_coef


##################################################
# Linear Regression Models
# (Continuous Dependent Variable)
##################################################

#--------------------------------------------------
# Generate sample data
#--------------------------------------------------

set.seed(42)

ind_lm_data <- gen_agg_lm(model = 'lm')

# Variables appear correct.
# All inside the unit interval.
summary(ind_lm_data)


# Aggregate the counts.
ind_lm_data[, 'num'] <- 1

# Add the squared responses for calculation of statistics.
ind_lm_data[, 'y_squared'] <- ind_lm_data[, 'y']^2
# For response 'name_of_response', this variable needs
# to be called 'name_of_response_squared'

agg_lm_data <- aggregate(cbind(num, y, y_squared) ~ x1 + x2 + x3,
                          data = ind_lm_data,
                          FUN = sum)

# Add a column for the mean of the y.
agg_lm_data[, 'y_bar'] <- agg_lm_data[, 'y']/agg_lm_data[, 'num']

summary(agg_lm_data)

# Test that this is an LPM.
all(agg_lm_data[, 'y'] %in% c(0, 1))


#--------------------------------------------------
# Estimate Linear Model from Individual Data
#--------------------------------------------------

ind_lm_reg <- lm(y ~ x1 + x2 + x3, data = ind_lm_data)

#--------------------------------------------------
# Estimate Linear Model from Aggregated Data
#--------------------------------------------------


# Modified lm function.
# agg_lm_reg <- agg_lm(y ~ x1 + x2 + x3, data = agg_lm_data, weights = num)
# agg_lm_reg <- agg_lm(y_bar ~ x1 + x2 + x3, data = agg_lm_data, weights = num)
agg_lm_reg <- agg_lm(y_bar ~ x1 + x2 + x3, data = agg_lm_data, weights = num,
                     y_squared = agg_lm_data[, 'y_squared'])
# This looks ugly but let's get it working first.


#--------------------------------------------------
# Compare the results
#--------------------------------------------------


# Compare summaries.
summary(ind_lm_reg)
summary_agg_lm(agg_lm_reg)


# Compare estimates first.
ind_lm_coef <- coef(ind_lm_reg)
agg_lm_coef <- coef(agg_lm_reg)
ind_lm_coef
agg_lm_coef
# Same.

# [1,] "coefficients"
ind_lm_reg$coefficients
agg_lm_reg$coefficients
# Same.

# [2,] "residuals"
summary(ind_lm_reg$residuals)
summary(agg_lm_reg$residuals)
# Not the same but ok.
summary(agg_lm_reg$residuals*agg_lm_reg$weights)
# Weighted residuals have mean zero.


# [4,] "rank"
ind_lm_reg$rank
agg_lm_reg$rank
# Same.


# [8,] "df.residual"
ind_lm_reg$df.residual
agg_lm_reg$df.residual
# Same.


# Compare summaries.
ind_lm_reg_summ <- summary(ind_lm_reg)
agg_lm_reg_summ <- summary_agg_lm(agg_lm_reg)


attributes(ind_lm_reg_summ)
# $`names`
# [1] "call"          "terms"         "residuals"     "coefficients"
# [5] "aliased"       "sigma"         "df"            "r.squared"
# [9] "adj.r.squared" "fstatistic"    "cov.unscaled"


# [1,] "call"
ind_lm_reg_summ$call
agg_lm_reg_summ$call
# Accurate.

# [2,] "terms"
ind_lm_reg_summ$terms
agg_lm_reg_summ$terms
# Accurate.

# [3,] "residuals"
summary(ind_lm_reg_summ$residuals)
summary(agg_lm_reg_summ$residuals)
summary(agg_lm_reg_summ$residuals*agg_lm_reg$weights)
# Works now. Previous version had sqrt(weights).
# summary(agg_lm_reg$residuals*agg_lm_reg$weights)
#
# summary(agg_lm_reg$residuals - agg_lm_reg_summ$residuals)
# # Not sure why these residuals don't match.
# # Try this:
# summary(agg_lm_reg_summ$residuals*sqrt(agg_lm_reg$weights))
# # Bingo.

# [4,] "coefficients"
ind_lm_reg_summ$coefficients
agg_lm_reg_summ$coefficients
# Standard errors need adjustment.
# Compare standrd errors.
ind_lm_reg_summ$coefficients[, 'Std. Error']
agg_lm_reg_summ$coefficients[, 'Std. Error']

agg_lm_reg_summ$coefficients[, 'Std. Error'] /
  ind_lm_reg_summ$coefficients[, 'Std. Error']
# Off by the same factor. The s^2 needs adjustment.
# Now they are correct.

# [5,] "aliased"
ind_lm_reg_summ$aliased
agg_lm_reg_summ$aliased
# Same.

# [6,] "sigma"
ind_lm_reg_summ$sigma
agg_lm_reg_summ$sigma
# Not equal but check ratio.
agg_lm_reg_summ$sigma / ind_lm_reg_summ$sigma
# Same ratio as for standard errors.
# (which is now correct).

# Correct sigma. Goal:
ind_lm_reg_summ$sigma
sqrt(sum(ind_lm_reg_summ$residuals^2)/ind_lm_reg$df.residual)
# Repeat for model on aggregated data.
sqrt(sum(agg_lm_reg$weights * agg_lm_reg_summ$residuals^2)/agg_lm_reg$df.residual)
# Reproduced (incorrect) calculated value.

# Reproduce RSS.
rss_check_ind <- sum(ind_lm_reg_summ$residuals^2)
w_A <- agg_lm_reg$weights
y_A <- agg_lm_data[, 'y_bar']
y2_A <- agg_lm_data[, 'y_squared']
f_A <- agg_lm_reg$fitted.values
# rss_check_agg <- sum(w_A * y_A) - 2*sum(w_A * y_A * f_A) + sum(w_A * f_A^2)
rss_check_agg <- sum(y2_A) - 2*sum(w_A * y_A * f_A) + sum(w_A * f_A^2)
# Bingo.

# [7,] "df"
ind_lm_reg_summ$df
agg_lm_reg_summ$df


# [8,] "r.squared"
ind_lm_reg_summ$r.squared
agg_lm_reg_summ$r.squared


# [9,] "adj.r.squared"
ind_lm_reg_summ$adj.r.squared
agg_lm_reg_summ$adj.r.squared


# [10,] "fstatistic"
ind_lm_reg_summ$fstatistic
agg_lm_reg_summ$fstatistic


# [11,] "cov.unscaled"
ind_lm_reg_summ$cov.unscaled
agg_lm_reg_summ$cov.unscaled
# Wierd. Border and diagonal same, off-diagonal not.
agg_lm_reg_summ$cov.unscaled / ind_lm_reg_summ$cov.unscaled



##################################################
# Logistic Regression Models
# (Binary Dependent Variable)
##################################################






##################################################
# End
##################################################
