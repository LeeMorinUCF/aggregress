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






##################################################
# Logistic Regression Models
# (Binary Dependent Variable)
##################################################






##################################################
# End
##################################################
