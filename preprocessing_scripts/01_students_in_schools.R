# Preprocessing pipeline for synthetic control analysis
# Input: raw data spss files
# Output: single data frame with all required info
# last edited 20230621 by @vankesteren
library(tidyverse)
library(haven)

# Debug: set NMAX to not infinite
NMAX <- Inf

# File paths ----
source("preprocessing_scripts/00_file_paths.R")

# Read data from INSCHRWPOTAB ----
cat(format(now()), "Reading school info and combining.\n")

# first, we make a table of school, year, student, and characteristics 
# where every row is a student in year 8 for the years 2011-2020
# read and combine all the wpo datasets, select only 8th year
read_school_8 <- function(path) {
  cat(path, "\r")
  read_sav(path, n_max = NMAX) |> 
    filter(RINPERSOON != "", WPOLEERJAAR == " 8")
}

# columns to extract
year8_students <- 
  lapply(PATHS$INSCHRWPO, read_school_8) |>
  bind_rows(.id = "peiljaar")

# extract columns, recode types
year8_students <-
  year8_students |> 
  select(RINPERSOONS,
         RINPERSOON,
         peiljaar,
         WPOBRIN_crypt,
         WPOBRINVEST,
         WPOADVIESVO,
         WPOVERBLIJFSJRINST,
         WPOTYPEPO,
         WPODENOMINATIE,
         WPOCODEEINDTOETS,
         WPOUITSLAGEINDTOETS,
         WPOGROEPSGROOTTE,
         WPOADVIESHERZ) |> 
  mutate(
    across(c(WPOBRIN_crypt, peiljaar), as_factor), 
    RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
    WPOBRINVEST = as_factor(WPOBRINVEST, levels = "values"),
    across(c(WPOVERBLIJFSJRINST, WPOUITSLAGEINDTOETS, WPOGROEPSGROOTTE), as.integer)
  )

# Exclude special education
year8_students <- 
  year8_students |> 
  filter(WPOTYPEPO != "SBO") |> 
  select(-WPOTYPEPO)

write_rds(year8_students, "scratch/year8_students_base.rds")

# Join with NPRZ treatment information ----
cat(format(now()), "Add intervention information.\n")

NPRZ_schools <-
  read_spss(PATHS$SCHOOL_INTERVENTION) |>
  mutate(
    across(starts_with("Hours"), as.numeric),
    children_zone = as_factor(Children_Zone),
  ) |>
  # we have two 1617 variables, a is most likely the one 
  # with less measurement error
  # TODO: potential robustness check - use b
  mutate(Hours_1617 = Hours_1617a) |>
  select(-Hours_1617a, -Hours_1617b, -Children_Zone)

# first, create intervention_school indicator
# TODO: exclude late schools from donor pool!
year8_students <- 
  year8_students |> 
  left_join(NPRZ_schools |> select(wpobrin_crypt, wpobrinvest, children_zone),
            c("WPOBRIN_crypt" = "wpobrin_crypt", "WPOBRINVEST" = "wpobrinvest")) |> 
  mutate(intervention = factor(
    case_when(
      is.na(children_zone)   ~ "no",
      children_zone == "Yes" ~ "early",
      children_zone == "No"  ~ "late"
    ),
    levels = c("no", "early", "late")
  ), .keep = "unused") |> 
  relocate(intervention, .after = WPOBRINVEST)

# then, also include how many hours for each peiljaar
NPRZ_schools_long <- 
  NPRZ_schools |> 
  pivot_longer(
    cols = starts_with("Hours"), 
    values_to = "intervention_hours",
    names_to = "peiljaar",
    names_pattern = "Hours_\\d{2}(\\d{2})", # use the second year as peiljaar 
    names_transform = \(y) as_factor(2000 + as.numeric(y))
  ) |> 
  select(-children_zone)

year8_students <- 
  year8_students |> 
  left_join(
    NPRZ_schools_long,
    by = c(
      "WPOBRIN_crypt" = "wpobrin_crypt",
      "WPOBRINVEST" = "wpobrinvest",
      "peiljaar"
    )
  ) |> 
  relocate(intervention_hours, .after = intervention)

write_rds(year8_students, "scratch/year8_students_intervention.rds")

# Compute outcome variables ----
cat(format(now()), "Computing outcome variables.\n")

# ISLED column mixed recommendation: take average of ISLEDs
# ISLEDHI column mixed recommendation: take highest ISLED
ISLED_LUT <- tribble(
  ~advies, ~ISLED, ~ISLEDHI, ~HAVOVWO, ~VMBO, ~KADER,
     "10",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "20",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "21",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "22",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "23",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "24",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "25",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "26",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "27",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "28",  45.82,    62.30,    FALSE, FALSE,  FALSE,
     "29",  50.63,    71.92,    FALSE, FALSE,  FALSE,
     "30",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "31",  29.34,    29.34,    FALSE,  TRUE,   TRUE,
     "32",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "33",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "34",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "35",  37.31,    45.27,    FALSE,  TRUE,  FALSE,
     "36",  45.82,    62.30,    FALSE, FALSE,  FALSE,
     "37",  50.63,    71.92,    FALSE, FALSE,  FALSE,
     "40",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "41",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "42",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "43",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "44",  53.79,    62.30,    FALSE, FALSE,  FALSE,
     "45",  58.60,    71.92,    FALSE, FALSE,  FALSE,
     "50",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "51",  45.27,    45.27,    FALSE,  TRUE,  FALSE,
     "52",  53.79,    62.30,    FALSE, FALSE,  FALSE,
     "53",  58.60,    71.92,    FALSE, FALSE,  FALSE,
     "60",  62.30,    62.30,     TRUE, FALSE,  FALSE,
     "61",  67.11,    71.92,     TRUE, FALSE,  FALSE,
     "70",  71.92,    71.92,     TRUE, FALSE,  FALSE
)

year8_students <- 
  year8_students |>
  left_join(ISLED_LUT, by = c("WPOADVIESVO" = "advies")) |>
  left_join(
    ISLED_LUT,
    by = c("WPOADVIESHERZ" = "advies"),
    suffix = c("_original", "_corr")
  ) |> 
  mutate(
    outcome_ISLED   = ifelse(is.na(ISLED_corr), ISLED_original, ISLED_corr), 
    outcome_HAVOVWO = ifelse(is.na(HAVOVWO_corr), HAVOVWO_original, HAVOVWO_corr), 
    outcome_VMBO    = ifelse(is.na(VMBO_corr), VMBO_original, VMBO_corr), 
    outcome_KADER   = ifelse(is.na(KADER_corr), KADER_original, KADER_corr), 
    .keep = "unused"
  )

# add CITO score
# and ensure that it is within the range of possible values
year8_students <- 
  year8_students |>
  mutate(
    outcome_CITO = if_else(WPOCODEEINDTOETS %in% c("01", "11"), WPOUITSLAGEINDTOETS, NA),
    outcome_CITO = if_else(outcome_CITO >= 500 & outcome_CITO <= 550, outcome_CITO, NA)
  )

write_rds(year8_students, "scratch/year8_students_outcome.rds")

# Compute control variables from INSCHRWPOTAB ----
cat(format(now()), "Computing covariates based on WPOTAB information.\n")

# add test type control variable
year8_students <- 
  year8_students |>
  mutate(
    control_testtype = factor(case_when(
      WPOCODEEINDTOETS == "01" ~ "CITO",
      WPOCODEEINDTOETS == "11" ~ "CITO",
      WPOCODEEINDTOETS == "12" ~ "ROUTE8",
      WPOCODEEINDTOETS == "13" ~ "IEP",
      WPOCODEEINDTOETS == ""   ~ NA,
      is.na(WPOCODEEINDTOETS)  ~ NA,
      .default = "other"
    ), levels = c("CITO", "ROUTE8", "IEP", "other"))
  )

# add denomination control variable
# TODO: check if this categorization is correct
year8_students <- 
  year8_students |> 
  mutate(denom_str = str_trim(WPODENOMINATIE)) |> 
  mutate(control_schooldenom = factor(
    case_when(
      denom_str %in% c("PC", "PCE", "PCR") ~ "Protestant",
      denom_str == "RK"                    ~ "Catholic",
      denom_str == "OPB"                   ~ "Public",
      denom_str == "ISL"                   ~ "Islamic",
      is.na(denom_str)                     ~ NA,
      .default = "other"
    ),
    levels = c(
      "Protestant",
      "Catholic",
      "Public",
      "Islamic",
      "other"
    )
  ))

# add school stability control variables
year8_students <- 
  year8_students |> 
  mutate(
    control_schoolstability_trunc = pmin(8, WPOVERBLIJFSJRINST),
    control_schoolstability_less6 = WPOVERBLIJFSJRINST < 6
  )

# add group size control variable, NB not available after 2017,
# and this variable can be quite unreliable according to CBS
year8_students <- 
  year8_students |> 
  mutate(control_groupsize = WPOGROEPSGROOTTE)

write_rds(year8_students, "scratch/year8_students_control_wpo.rds")

# Compute descent variables from GBAPERSOONTAB ----
cat(format(now()), "Loading and joining GBAPERSOONTAB information.\n")
gba_tab <- 
  read_spss(
    PATHS$GBA, 
    n_max = NMAX * 6, 
    col_select = c("RINPERSOON", "RINPERSOONS", "GBAHERKOMSTGROEPERING")
  ) |> 
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"))

# NB: you can check these codes as follows: 
# country_codes[country_codes %in% aru_ant_codes]
country_codes <- attr(gba_tab$GBAHERKOMSTGROEPERING, "labels")
aru_ant_codes <- c("7011", "5095", "5016", "5107", "5109", "5108", "5110")
me_eur_codes  <- c("7028", "5017", "6066", "7048", "6067", "5049", "7065", 
                   "7064", "7066", "7024", "7047")

year8_students <- 
  year8_students |> 
  left_join(gba_tab, by = c("RINPERSOONS", "RINPERSOON")) |> 
  mutate(
    control_desc_native  = GBAHERKOMSTGROEPERING == "6030",
    control_desc_turkey  = GBAHERKOMSTGROEPERING == "6043",
    control_desc_morocco = GBAHERKOMSTGROEPERING == "5022",
    control_desc_surinam = GBAHERKOMSTGROEPERING == "5007",
    control_desc_aru_ant = GBAHERKOMSTGROEPERING %in% aru_ant_codes,
    control_desc_me_eur  = GBAHERKOMSTGROEPERING %in% me_eur_codes,
    control_desc_syria   = GBAHERKOMSTGROEPERING == "7009",
    control_desc_eritrea = GBAHERKOMSTGROEPERING == "9003",
    control_desc_capever = GBAHERKOMSTGROEPERING %in% c("8025", "5067"),
    control_desc_missing = 
      is.na(GBAHERKOMSTGROEPERING) | 
      GBAHERKOMSTGROEPERING %in% c("----", "0000")
  ) |> 
  select(-GBAHERKOMSTGROEPERING)

rm(gba_tab)

# Write to scratch ----
cat(format(now()), "Storing result to scratch.\n")
write_rds(year8_students, "scratch/year8_students_descent.rds")

warnings()

