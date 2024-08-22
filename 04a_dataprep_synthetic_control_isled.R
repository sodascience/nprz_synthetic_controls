# Script to prepare matrices for synthetic control analysis
# i.e., X1, X0, Z1, Z0, and v weights for each school.
# last edited 2024-01-26 by @vankesteren
library(tidyverse)
library(mice)
library(purrr)

# Read prepared data
df <- read_rds("processed_data/school_data.rds")

# Remove late intervention group
df <- df |> filter(intervention != "late")

# Get intervention schools
intervened_ids <- 
  df |> 
  filter(intervention != "no") |> 
  pull(school_id) |> 
  unique()


# Year should be integer and schoolstability should be too
df <- 
  df |> 
  mutate(
    peiljaar = as.integer(as.character(peiljaar)),
    control_schoolstability_trunc = as.integer(as.character(control_schoolstability_trunc))
  )

# make sure every peiljaar is there
df <- expand_grid(
  peiljaar = 2009L:2022L, 
  school_id = unique(df$school_id),
) |> left_join(df, by = join_by(school_id, peiljaar))


# Make sure intervention column is complete
df[df$school_id %in% intervened_ids & is.na(df$intervention), "intervention"] <- "early"
df[!df$school_id %in% intervened_ids & is.na(df$intervention), "intervention"] <- "no"


# only retain the variables we need
df <- 
  df |> 
  select(
    school_id,
    intervention, 
    peiljaar,
    outcome_ISLED_mean,
    outcome_ISLED_valid,
    outcome_ISLED_missing,
    control_schoolsize,
    control_desc_native_mean,
    control_desc_surinam_mean,
    control_desc_morocco_mean,
    control_desc_turkey_mean,
    control_desc_aru_ant_mean,
    control_perc_income_mean,
    control_perc_wealth_mean,
    control_single_parent_mean,
    control_educ_lower_pa_mean,
    control_educ_lower_ma_mean,
    control_schooldenom,
    control_schoolstability_trunc
  )

# Missing data handling
df <-
  df |>
  mutate(
    prop_valid_isled = outcome_ISLED_valid / (outcome_ISLED_missing + outcome_ISLED_valid),
    outcome_ISLED_mean = if_else(prop_valid_isled < 0.75 | outcome_ISLED_valid < 11, NA, outcome_ISLED_mean),
  )

# Remove schools with too few data-points
# Schools should have at least 2 pre-intervention ISLED scores and 2 post
keep_schools <-
  df |> 
  mutate(prepost = if_else(peiljaar <= 2013, "pre", "post")) |>
  summarize(count = sum(!is.na(outcome_ISLED_mean)), .by = c(school_id, prepost)) |>
  arrange(school_id) |>
  summarize(keep = min(count) >= 2, .by = school_id)

df <- left_join(df, keep_schools, by = join_by(school_id)) |> filter(keep) |> 
  select(-keep, -prop_valid_isled, -outcome_ISLED_valid, -outcome_ISLED_missing)


# Single imputation of the remaining missings. NB: single imputation will ignore some 
# level of uncertainty about the values we impute. 
res <- mice(df, m = 1)
df  <- as_tibble(complete(res, 1))

# function to create synth matrices from data frame and ids.
create_synth_matrices <- function(df, intervened_ids, id = 1) {
  intervened_id <- intervened_ids[id]
  i_school <- df |> filter(school_id == intervened_id)
  
  if (nrow(i_school) == 0) return()
  
  # only retain donors that are the same denomination as the i_school 
  donor_ids <- 
    df |> 
    filter(!school_id %in% intervened_ids) |> 
    summarize(
      donor = i_school$control_schooldenom[1] %in% control_schooldenom, 
      .by = school_id
    ) |> 
    filter(donor) |> 
    select(school_id)
  d_schools <- df |> inner_join(donor_ids, by = join_by(school_id))
  
  id_schools <- bind_rows(i_school, d_schools)
  
  # Compute X0, X1
  # create pre-intervention covariates for entire pre-intervention window
  X_data <- 
    id_schools |> 
    filter(peiljaar <= 2013) |> 
    summarise(
      schoolsize      = mean(control_schoolsize, na.rm = TRUE),
      desc_native     = mean(control_desc_native_mean, na.rm = TRUE),
      desc_surinam    = mean(control_desc_surinam_mean, na.rm = TRUE),
      desc_morocco    = mean(control_desc_morocco_mean, na.rm = TRUE),
      desc_turkey     = mean(control_desc_turkey_mean, na.rm = TRUE),
      desc_aru_ant    = mean(control_desc_aru_ant_mean, na.rm = TRUE),
      perc_income     = mean(control_perc_income_mean, na.rm = TRUE),
      single_parent   = mean(control_single_parent_mean, na.rm = TRUE),
      schoolstability = mean(control_schoolstability_trunc, na.rm = TRUE),
      .by = school_id
    )
  
  # create education_lower covariates
  X_data <- 
    X_data |> 
    left_join(
      id_schools |> 
      filter(peiljaar %in% 2012:2013) |> 
      summarise(
        edu_lo_father  = mean(control_educ_lower_pa_mean, na.rm = TRUE),
        edu_lo_mother  = mean(control_educ_lower_ma_mean, na.rm = TRUE),
        .by = school_id
      ),
      by = join_by(school_id)
    )
  
  X1 <- X_data |> filter(school_id == intervened_id) |> select(-school_id) |> t()
  colnames(X1) <- intervened_id
  
  X0 <- X_data |> filter(school_id != intervened_id) |> select(-school_id) |> t()
  colnames(X0) <- X_data |> filter(school_id != intervened_id) |> pull(school_id)
  
  # create outcome matrices
  outcome_data <- 
    id_schools |> 
    summarise(
      outcome = outcome_ISLED_mean,
      .by = c(school_id, peiljaar)
    )
  
  # Pre-intervention outcomes
  Z_data <- 
    outcome_data |> 
    filter(peiljaar <= 2013) |> 
    pivot_wider(
      names_from = peiljaar, 
      values_from = outcome, 
      names_prefix = "outcome_"
    )
  
  Z1 <- Z_data |> filter(school_id == intervened_id) |> select(-school_id) |> t()
  colnames(Z1) <- intervened_id
  
  Z0 <- Z_data |> filter(school_id != intervened_id) |> select(-school_id) |> t()
  colnames(Z0) <- Z_data |> filter(school_id != intervened_id) |> pull(school_id)
  
  # create post-intervention outcome
  Y_data <- 
    outcome_data |> 
    filter(peiljaar > 2013) |> 
    pivot_wider(
      names_from = peiljaar, 
      values_from = outcome, 
      names_prefix = "outcome_"
    )
  
  Y1 <- Y_data |> filter(school_id == intervened_id) |> select(-school_id) |> t()
  colnames(Y1) <- intervened_id
  
  Y0 <- Y_data |> filter(school_id != intervened_id) |> select(-school_id) |> t()
  colnames(Y0) <- Y_data |> filter(school_id != intervened_id) |> pull(school_id)
  
  return(list(
    X1 = X1, # pre-intervention covariates, treated unit
    X0 = X0, # pre-intervention covariates, donor pool
    Z1 = Z1, # pre-intervention outcome, treated unit
    Z0 = Z0, # pre-intervention outcome, donor pool
    Y1 = Y1, # post-intervention outcome, treated unit
    Y0 = Y0  # post-intervention outcome, donor pool
  ))
}

# Actually create the synth matrices
synth_mats <- map(
  .x = seq_along(intervened_ids), 
  .f = function(i) create_synth_matrices(df, intervened_ids, i), 
  .progress = TRUE
)
names(synth_mats) <- intervened_ids

# remove empty ones
synth_mats <- synth_mats[map(synth_mats, length) != 0]

# store the synth matrices
write_rds(synth_mats, "processed_data/synth_mats_isled.rds")
