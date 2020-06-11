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
# Junw 10, 2020
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
#
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

ind_lpm_data <- gen_agg_lm(model = 'lpm')

# Variables appear correct.
# All inside the unit interval.
summary(ind_lpm_data)


# Aggregate the counts.
ind_lpm_data[, 'num'] <- 1

agg_lpm_data <- aggregate(num ~ y + x1 + x2 + x3,
                      data = ind_lpm_data,
                      FUN = sum)



#--------------------------------------------------
# Estimate Linear Model from Individual Data
#--------------------------------------------------

ind_lpm_lm <- lm(y ~ x1 + x2 + x3, data = ind_lpm_data)

#--------------------------------------------------
# Estimate Linear Model from Aggregated Data
#--------------------------------------------------


# Modified lm function.
agg_lpm_lm <- agg_lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num)


#--------------------------------------------------
# Compare the results
#--------------------------------------------------


# Compare summaries.
summary(ind_lpm_lm)
summary_agg_lm(agg_lpm_lm)



# Goal is to make sure that the lm objects are the same.
# The estimates should be the same.
# However, the data stored will be the same size as the aggregated
# data -- and that's the point.


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


# [4,] "rank"
ind_lpm_lm$rank
agg_lpm_lm$rank
# Same.


# [8,] "df.residual"
ind_lpm_lm$df.residual
agg_lpm_lm$df.residual
# Same.




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


# [4,] "coefficients"
ind_lm_reg_summ$coefficients
agg_lm_reg_summ$coefficients
# Standard errors need adjustment.
# Compare standrd errors.
ind_lm_reg_summ$coefficients[, 'Std. Error']
agg_lm_reg_summ$coefficients[, 'Std. Error']

agg_lm_reg_summ$coefficients[, 'Std. Error'] /
  ind_lm_reg_summ$coefficients[, 'Std. Error']

# [6,] "sigma"
ind_lm_reg_summ$sigma
agg_lm_reg_summ$sigma


# [7,] "df"
ind_lm_reg_summ$df
agg_lm_reg_summ$df
# Same.



# [8,] "r.squared"
ind_lm_reg_summ$r.squared
agg_lm_reg_summ$r.squared
# Same.

# [9,] "adj.r.squared"
ind_lm_reg_summ$adj.r.squared
agg_lm_reg_summ$adj.r.squared


# [10,] "fstatistic"
ind_lm_reg_summ$fstatistic
agg_lm_reg_summ$fstatistic



##################################################
# Logistic Regression Models
# (Binary Dependent Variable)
##################################################






##################################################
# End
##################################################
