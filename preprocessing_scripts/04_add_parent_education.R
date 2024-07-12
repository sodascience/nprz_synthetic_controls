library(tidyverse)
library(haven)

# Debug: set NMAX to not infinite
NMAX <- Inf

# File paths ----
source("preprocessing_scripts/00_file_paths.R")

# Read data ---- 
cat(format(now()), "Reading current year8 students file.\n")
year8_students <- read_rds("scratch/year8_students_hhincome.rds")

# Kindoudertab links child to parent
cat(format(now()), "Reading kindoudertab file.\n")
kindoudertab <- read_sav(
  file = PATHS$PARENT_TAB,
  col_select = c("RINPERSOONS", "RINPERSOON", "RINPERSOONSpa", "RINPERSOONpa", "RINPERSOONSMa", "RINPERSOONMa"),
  n_max = NMAX
) |> 
  mutate(
    RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"),
    RINPERSOONSMa  = as_factor(RINPERSOONSMa, levels = "values"),
    RINPERSOONSpa  = as_factor(RINPERSOONSpa, levels = "values"),
    RINPERSOONMa   = na_if(RINPERSOONMa, "---------"),
    RINPERSOONpa   = na_if(RINPERSOONpa, "---------"),
  )

# add to base table
cat(format(now()), "Joining kindoudertab with students.\n")
year8_students <- 
  year8_students |> 
  left_join(kindoudertab, by = join_by(RINPERSOONS, RINPERSOON))

# remove for memory efficiency
rm(kindoudertab)

# Read hoogsteopltab for each year
cat(format(now()), "Reading highest education file.\n")
all_parents_id <- 
  na.omit(unique(c(year8_students$RINPERSOONpa, year8_students$RINPERSOONMa)))

edu_cols <- c("RINPERSOONS","RINPERSOON","OPLNRHB")

read_edu <- function(path) {
  cat(path, "\r")
  res <- 
    read_sav(path, n_max = NMAX, col_select = all_of(edu_cols)) |> 
    filter(RINPERSOON %in% all_parents_id) |> 
    mutate(
      RINPERSOONS = as_factor(RINPERSOONS, levels = "values")
    )
  if (inherits(res$OPLNRHB, "haven_labelled")) {
    res <- res |> mutate(OPLNRHB = as_factor(OPLNRHB, levels = "values"))
  } else {
    res <- res |> mutate(OPLNRHB = factor(OPLNRHB))
  }
  return(res)
}

cat(format(now()), "Reading education table.\n")
education <- 
  lapply(PATHS$HIGHEST_EDUCATION, read_edu) |> 
  bind_rows(.id = "peiljaar")

# add education level to education table
cat(format(now()), "Reading education level lookup table.\n")
edu_lut <- 
  read_sav(
    PATHS$EDUCATION_LUT, 
    col_select = c("OPLNR", "ONR2019NIVEAU")
  ) |> 
  mutate(OPLNR = as_factor(OPLNR, levels = "value"),
         ONR2019NIVEAU = as.integer(as.character(ONR2019NIVEAU)))

education <- 
  education |> 
  left_join(edu_lut, by = join_by(OPLNRHB == OPLNR))

# Join to the students file ----
cat(format(now()), "Joining parent education level with students table.\n")
year8_students <- 
  year8_students |>
  left_join(
    education,
    by = join_by(
      RINPERSOONSpa == RINPERSOONS,
      RINPERSOONpa == RINPERSOON,
      peiljaar
    )
  ) |>
  left_join(
    education,
    by = join_by(
      RINPERSOONSMa == RINPERSOONS,
      RINPERSOONMa == RINPERSOON,
      peiljaar
    ),
    suffix = c("_pa", "_ma")
  )

# Recode the highest education level variable ----
cat(format(now()), "Computing education level variables.\n")
year8_students <-
  year8_students |> 
  mutate(
    control_educ_higher_pa  = ONR2019NIVEAU_pa > 30   & ONR2019NIVEAU_pa < 33,
    control_educ_higher_ma  = ONR2019NIVEAU_ma > 30   & ONR2019NIVEAU_ma < 33,
    control_educ_lower_pa   = ONR2019NIVEAU_pa > 10   & ONR2019NIVEAU_pa < 14,
    control_educ_lower_ma   = ONR2019NIVEAU_ma > 10   & ONR2019NIVEAU_ma < 14,
    control_educ_missing_pa = is.na(ONR2019NIVEAU_pa) | ONR2019NIVEAU_pa == 99,
    control_educ_missing_ma = is.na(ONR2019NIVEAU_ma) | ONR2019NIVEAU_ma == 99
  )

# Write to scratch ----
cat(format(now()), "Storing result to scratch.\n")
write_rds(year8_students, "scratch/year8_students_peducation.rds")
