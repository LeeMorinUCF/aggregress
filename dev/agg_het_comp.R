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
# June 19, 2020
#
##################################################
#
# agg_het_comp demonstrates a comparison of analysis
#   using aggregated and otherwise identical
#   pre-aggregated data, with adjustments for
#   heteroskedastic error terms.
#
# Dependencies:
#   The aggregress package and the sandwich package
#   to calculate the HCCME with unaggregated data.
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

ind_lpm_lm <- lm(y ~ x1 + x2 + x3, data = ind_lpm_data,
                 x = TRUE)

# Correct the satandard errors for heteroskedasticity.
ind_lpm_hccme <- white_hccme(ind_lpm_lm)

# Compare the homoskedastic with the heteroskedastic cases.
summary(ind_lpm_lm)
print(ind_lpm_hccme$coef_hccme)
# Looks plausible.
# Compare it to reg y x, robust?

# Recalculate from first principles.
ind_lpm_hccme_slow <- white_hccme_slow(ind_lpm_lm)
print(ind_lpm_hccme_slow$coef_hccme)
# Exactly the same.
# Check for accuracy of SEs alone:
ind_lpm_hccme_slow$coef_hccme[, 'Std. Error'] /
  ind_lpm_hccme$coef_hccme[, 'Std. Error']
# Exactly the same.




#--------------------------------------------------
# Estimate Linear Model from Aggregated Data
#--------------------------------------------------


# Modified lm function.
agg_lpm_lm <- agg_lm(y ~ x1 + x2 + x3, data = agg_lpm_data, weights = num,
                     x = TRUE)


# Correct the satandard errors for heteroskedasticity.
# agg_lpm_hccme <- white_hccme(agg_lpm_lm)
# Throws an error about a mystery variable y.
agg_lpm_hccme_slow <- white_hccme_slow(agg_lpm_lm)
agg_lpm_hccme_med <- white_hccme_med(agg_lpm_lm)




#--------------------------------------------------
# Compare the results
#--------------------------------------------------


# Compare summaries.

# Homoskedastic case.
summary(ind_lpm_lm)
summary_agg_lm(agg_lpm_lm)

# Heteroskedastic case.
# Un-aggregated case (individual data).
print(ind_lpm_hccme$coef_hccme)
print(ind_lpm_hccme_slow$coef_hccme)
# Aggregated data.
print(agg_lpm_hccme_slow$coef_hccme)
print(agg_lpm_hccme_med$coef_hccme)

# Check for accuracy of SEs alone:
ind_lpm_hccme_slow$coef_hccme[, 'Std. Error'] /
  ind_lpm_hccme$coef_hccme[, 'Std. Error']

agg_lpm_hccme_slow$coef_hccme[, 'Std. Error'] /
  ind_lpm_hccme$coef_hccme[, 'Std. Error']
# Correctomundo!

agg_lpm_hccme_med$coef_hccme[, 'Std. Error'] /
  ind_lpm_hccme$coef_hccme[, 'Std. Error']
# Also correctomundo.



