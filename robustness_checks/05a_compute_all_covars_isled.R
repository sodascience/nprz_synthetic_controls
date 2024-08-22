# Script to compute synthetic control weights
# i.e., X1, X0, Z1, Z0, and v weights for each school.
# last edited 2024-01-26 by @vankesteren
library(tidyverse)
library(furrr)
plan(multisession)
library(pensynth) # github.com/vankesteren/pensynth

# read the prepared data
synth_mats <- read_rds("processed_data/synth_mats_robustness_isled.rds")

# Compute penalized synthetic control
# use Z (pre-intervention outcomes) as cross-validation target to tune lambda
# and equally weigh pre-intervention covariates on a standardized scale
psynth_list <- future_map(
  .x = synth_mats, 
  .f = function(m) {
    cv_pensynth(
      X1 = m$X1,
      X0 = m$X0,
      v  = 1,
      Z1 = m$Z1,
      Z0 = m$Z0,
      nlambda = 100
    )
  }, 
  .progress = TRUE
)
write_rds(psynth_list, "processed_data/psynth_list_robustness_isled.rds")
