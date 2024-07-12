# Join household income info with sample & compute variables
# last edited 20230809 by @vankesteren
library(tidyverse)
library(haven)

# Debug: set NMAX to not infinite
NMAX <- Inf

# File paths ----
source("preprocessing_scripts/00_file_paths.R")

# Read data ----
cat(format(now()), "Reading current year8 students file.\n")
year8_students <- read_rds("scratch/year8_students_household.rds")

# Define functions for reading koppel, inha, and vehtab
koppel_cols <- c("RINPERSOONS", "RINPERSOON", "RINPERSOONSHKW", "RINPERSOONHKW")
inha_cols   <- c("RINPERSOONSHKW", "RINPERSOONHKW", "INHBBIHJ", "INHP100HGEST")
ihi_cols    <- c("RINPERSOONSKERN", "RINPERSOONKERN", "BVRBBIHJ", "BVRPERCGESTINKH")
vehtab_cols <- c("RINPERSOONSHKW", "RINPERSOONHKW", "VEHP100HVERM")

read_koppel <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(koppel_cols)) |> 
    filter(RINPERSOON %in% year8_students$RINPERSOON) |> 
    mutate(
      RINPERSOONS    = as_factor(RINPERSOONS, levels = "values"),
      RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values")
    )
}

read_inha <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(inha_cols)) |> 
    filter(RINPERSOONHKW %in% koppel$RINPERSOONHKW) |> 
    mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values"))
}

read_ihi <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(ihi_cols)) |> 
    filter(RINPERSOONKERN %in% koppel$RINPERSOONHKW) |>
    mutate(RINPERSOONSKERN = as_factor(RINPERSOONSKERN, levels = "values")) |> 
    rename(
      RINPERSOONSHKW = RINPERSOONSKERN,
      RINPERSOONHKW = RINPERSOONKERN,
      INHBBIHJ = BVRBBIHJ,
      INHP100HGEST = BVRPERCGESTINKH
    )
}

read_vehtab <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX, col_select = all_of(vehtab_cols)) |> 
    filter(RINPERSOONHKW %in% koppel$RINPERSOONHKW) |> 
    mutate(RINPERSOONSHKW = as_factor(RINPERSOONSHKW, levels = "values"))
}

# read koppel, ihi, and inha
cat(format(now()), "Reading koppel table.\n")
koppel <- 
  lapply(PATHS$KOPPELHH, read_koppel) |> 
  bind_rows(.id = "peiljaar")

cat(format(now()), "Reading IHI table.\n")
ihi <- 
  lapply(PATHS$IHI, read_ihi) |>
  bind_rows(.id = "peiljaar") |> 
  mutate(INHP100HGEST = as.integer(as.character(INHP100HGEST)))


cat(format(now()), "Reading INHA table.\n")
inha <- 
  lapply(PATHS$INHA, read_inha) |>
  bind_rows(.id = "peiljaar") |> 
  mutate(INHP100HGEST = as.integer(as.character(INHP100HGEST)))

cat(format(now()), "Binding IHI and INHA.\n")
inha <- bind_rows(ihi, inha)
rm(ihi)

# join koppel & inha ----
cat(format(now()), "Joining koppel & inha.\n")
inha_student <- 
  left_join(koppel, inha, by = join_by(peiljaar, RINPERSOONSHKW, RINPERSOONHKW)) |> 
  select(RINPERSOONS, RINPERSOON, peiljaar, INHBBIHJ, INHP100HGEST)

cat(format(now()), "Joining INHA with year8 students file.\n")
# join inha with students
year8_students <-
  year8_students |> 
  left_join(inha_student, by = join_by(peiljaar, RINPERSOONS, RINPERSOON))

# remove inha
rm(inha, inha_student)

# read vehtab ----
cat(format(now()), "Reading VEH table.\n")
vehtab <- 
  lapply(PATHS$VEHTAB, read_vehtab) |> 
  bind_rows(.id = "peiljaar") |> 
  mutate(VEHP100HVERM = as.integer(as.character(VEHP100HVERM)))

# join koppel & vehtab
cat(format(now()), "Joining VEH table with students.\n")
vehtab_student <- 
  left_join(koppel, vehtab, by = join_by(peiljaar, RINPERSOONSHKW, RINPERSOONHKW)) |> 
  select(RINPERSOONS, RINPERSOON, peiljaar, VEHP100HVERM) 


# join vehtab with students
year8_students <-
  year8_students |> 
  left_join(vehtab_student, by = join_by(peiljaar, RINPERSOONS, RINPERSOON))

# remove vehtab
rm(vehtab_student, vehtab)

# Compute income and wealth variables ----
cat(format(now()), "Computing income and wealth variables.\n")
year8_students <-
  year8_students |> 
  # compute household income
  # NB: negative values mean unknown income.
  # TODO: what does 0 mean?
  mutate(
    control_perc_income = if_else(INHP100HGEST < 1, NA, INHP100HGEST)
  ) |> 
  # compute household wealth
  # NB: negative values mean unknown wealth.
  mutate(
    control_perc_wealth = if_else(VEHP100HVERM < 1, NA, VEHP100HVERM)
  ) |> 
  # compute income source
  # scores 21-24 are varios forms of social assistance
  mutate(
    INHBBIHJ = na_if(INHBBIHJ, "99"),
    INHBBIHJ = as.integer(as.character(as_factor(INHBBIHJ, levels = "values"))),
    control_income_social = INHBBIHJ > 20 & INHBBIHJ < 25
  )

# write to scratch ----
cat(format(now()), "Writing hhincome file to scratch.\n")
write_rds(year8_students, "scratch/year8_students_hhincome.rds")
