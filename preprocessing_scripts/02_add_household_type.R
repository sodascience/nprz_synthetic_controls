# Join huishoudbus with sample & compute variables
# last edited 20230809 by @vankesteren
library(tidyverse)
library(haven)

# Debug: set NMAX to not infinite
NMAX <- Inf

# File paths ----
source("preprocessing_scripts/00_file_paths.R")

# Read data ----
cat(format(now()), "Reading current year8 students file.\n")
year8_students <- read_rds("scratch/year8_students_descent.rds")


cat(format(now()), "Reading Household info.\n")
hhbus_cols <- c("RINPERSOONS", "RINPERSOON", "DATUMAANVANGHH",
                "DATUMEINDEHH", "TYPHH")
hhbus <- 
  read_sav(PATHS$HHBUS, n_max = NMAX, col_select = all_of(hhbus_cols)) |> 
  mutate(
    RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
    date_range = interval(ymd(DATUMAANVANGHH), ymd(DATUMEINDEHH)), 
    .keep = "unused"
  )

# Join HHBUS with year8 students ----
cat(format(now()), "Joining household info with students.\n")

# only keep the relevant year
hh_selection <- 
  year8_students |> 
  select(RINPERSOONS, RINPERSOON, peiljaar) |> 
  mutate(peildatum = ymd(paste0(peiljaar, "0101"))) |> 
  left_join(hhbus, by = join_by(RINPERSOONS, RINPERSOON)) |> 
  filter(peildatum %within% date_range) |> 
  select(-peildatum, -date_range)

# remove hhbus
rm(hhbus)

# join with original year8_students
year8_students <- 
  year8_students |> 
  left_join(hh_selection, by = join_by(RINPERSOONS, RINPERSOON, peiljaar))

# remove selection
rm(hh_selection)

# Compute single parent household variable ----
cat(format(now()), "Computing single parent household variable.\n")
year8_students <- 
  year8_students |> 
  mutate(
    control_single_parent = TYPHH == "6",
    .keep = "unused"
  )

# Write to scratch ----
cat(format(now()), "Storing result to scratch.\n")
write_rds(year8_students, "scratch/year8_students_household.rds")

