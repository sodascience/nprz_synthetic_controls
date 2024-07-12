library(tidyverse)
library(haven)

cat(format(now()), "### COLLECTING YEAR8 STUDENTS ###\n")
cat(format(now()), "###     ETA +- 15 minutes     ###\n")
source("preprocessing_scripts/01_students_in_schools.R")
rm(list = ls())

cat(format(now()), "### ADDING HOUSEHOLD TYPE ###\n")
cat(format(now()), "###   ETA +- 30 minutes   ###\n")
source("preprocessing_scripts/02_add_household_type.R")
rm(list = ls())

cat(format(now()), "### ADDING HOUSEHOLD INCOME AND WEALTH ###\n")
cat(format(now()), "###          ETA +- 45 minutes         ###\n")
source("preprocessing_scripts/03_add_household_income_wealth.R")
rm(list = ls())

cat(format(now()), "### ADDING PARENT EDUCATION ###\n")
cat(format(now()), "###    ETA +- 15 minutes    ###\n")
source("preprocessing_scripts/04_add_parent_education.R")
rm(list = ls())

cat(format(now()), "### MOVING FILES AND CLEANING UP ###\n")
cat(format(now()), "###        ETA +- 1 minute       ###\n")
source("preprocessing_scripts/05_cleanup.R")
rm(list = ls())

cat(format(now()), "### DONE ###\n")
