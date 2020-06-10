##################################################
#
# aggregress Scratchpad
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# February 15, 2020
#
##################################################
#
# A set of commands to initialize the aggregress package
#
# Dependencies:
#   None.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))




# Check that the package development tools are installed correctly
library(devtools)
# Loading required package: usethis
# Warning messages:
#   1: package 'devtools' was built under R version 3.5.3
#   2: package 'usethis' was built under R version 3.5.3
has_devel()
# Your system is ready to build packages!


coverage_path <- '~/Research/TransUnion/Data/fi_level'

# Initialize the new package.
devtools::create('~/Documents/Research/aggregress/aggregress')
# The aggregress package in the aggregress repo in the aggregress folder.

# Next steps:
# Edit field in the description
# Create a function in the R folder.
# Add roxygen comments before the function.

# Use roxygen to build documentation.
devtools::document()



# Set some folders to be ignored by R build.
usethis::use_build_ignore(c("README.md", ".gitignore", "dev"))
# > usethis::use_build_ignore(c("README.md", ".gitignore", "dev"))
# check Setting active project to 'C:/Users/le279259/Documents/Research/aggregress/aggregress'
# check Adding '^README\\.md$', '^\\.gitignore$', '^dev$' to '.Rbuildignore'





##################################################
# End
##################################################


