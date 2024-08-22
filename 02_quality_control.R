# Quality control for synthetic control analysis
# Checking distributions, missings, summary tables, odd things
# last edited 20230901 by @custers

library(tidyverse)
library(haven)


y8s <- read_rds("processed_data/year8_students.rds")
y8s_full <- read_rds("processed_data/year8_students_full.rds")

# Check denomination of early intervention schools ----
y8s_full |>  
  filter(intervention == "early") |> 
  group_by(WPOBRIN_crypt, WPOBRINVEST, peiljaar) |>
  summarize(denom = list(table(WPODENOMINATIE)),
            rec_denom = list(table(control_schooldenom))) |> 
  unnest_wider(denom) |> 
  unnest_wider(rec_denom) |>
  View()

# check number of students per year ----
y8s |> summarize(n = length(unique(WPOBRIN_crypt)), .by = peiljaar)
table(y8s$peiljaar) 
# number of students increases until 2013, then decreases
# in 2009 the amount is substantially lower => indication of missings

# number of intervention schools per year ----
y8s |>
  filter(intervention == "early") |> 
  select(peiljaar,
         WPOBRIN_crypt,
         WPOBRINVEST) |>
  unique() |> 
  select(peiljaar) |>
  table()
# 24 schools for later years, only 18 for first year.
# we should have 26 in total. TODO: check where students/schools go missing
# They are simply not in the INSCHRWPOTAB
# 
# notfound <- 
#   anti_join(NPRZ_schools, y8s, by = join_by( "wpobrin_crypt" == "WPOBRIN_crypt", "wpobrinvest" == "WPOBRINVEST")) |> 
#   arrange(Hours_1314)
# not available: row 3, row 5
# brin_crypt: "7a14e2a0e3869f91ecb036df96cad418" "245e04f7f1165ee8c1e89c24704f7fa7"
# brinvest: "01" "01"

# checks for outcome variables ----
## Time-series plot of outcomes ----
outcomes_per_school <- 
  y8s |> 
  summarize(
    across(starts_with("outcome_"), \(x) mean(x, na.rm = TRUE)),
    .by = c(WPOBRIN_crypt, WPOBRINVEST, peiljaar, intervention)
  ) |> 
  pivot_longer(
    cols = starts_with("outcome_"), 
    names_to = "outcome", 
    values_to = "value", 
    names_prefix = "outcome_",
    names_transform = factor
  )

# create a plot for each outcome
for (outc in levels(outcomes_per_school$outcome)) {
  cat(outc, "\r")
  plt <- 
    outcomes_per_school |> 
    filter(outcome == outc, !is.na(value)) |> 
    arrange(intervention) |> 
    ggplot(aes(
      x = peiljaar, 
      y = value, 
      group = paste(WPOBRIN_crypt, WPOBRINVEST), 
      colour = intervention
    )) +
    geom_line() + 
    scale_colour_manual(values = c(
      "no"    = "#00000003", 
      "early" = "darkorange", 
      "late"  = "brown"
    )) +
    theme_minimal() +
    facet_wrap(vars(outcome), scales = "free") +
    labs(
      title = paste(outc, "in different schools over time."),
      y = outc,
      x = ""
    )
  fname <- paste0("img/outcome_timeseries_", outc, ".png")
  ggsave(fname, plt, width = 15, height = 10, bg = "white")
}

## missings analysis ----
### missings per year ----
y8s  |>
  group_by(peiljaar, outcome_ISLED) |>
  summarize(N_cat = n()) |>
  mutate(missing_perc = N_cat / sum(N_cat) * 100) |>
  filter(is.na(outcome_ISLED)) |>
  select(peiljaar, missing_perc) |>
  ggplot(aes(x = peiljaar, y = missing_perc)) +
  geom_bar(stat = "identity")
# Many missings in 2009 (>40%), gradually decreases until few missings in 2013 and after

# Make WPO_BRIN_unique for convenience of quality control
# TODO: check with EJ and Oisin how this might be done easier
# Has to with fillings cells that contain zero's 
y8s <- y8s |>
  unite(WPO_BRIN_unique,
        WPOBRIN_crypt,
        WPOBRINVEST,
        sep = "_",
        remove = FALSE)

y8s_full <- y8s_full |>
  unite(WPO_BRIN_unique,
        WPOBRIN_crypt,
        WPOBRINVEST,
        sep = "_",
        remove = FALSE)

# distribution of missings across schools for all years
y8s  |>
  group_by(peiljaar, WPO_BRIN_unique, outcome_ISLED) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar, WPO_BRIN_unique, outcome_ISLED, fill = list(N_cat = 0)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  mutate(missing_perc = N_cat / sum(N_cat) * 100) |>
  filter(is.na(outcome_ISLED) & !is.nan(missing_perc)) |>
  ungroup() |>
  select(peiljaar, missing_perc) |>
  ggplot(aes(x = missing_perc)) +
  geom_histogram(bins = 60) +
  facet_grid(peiljaar ~ .)
# schools with completely missing data (+- 100%) mostly in 2009 and 2010.  

# how many intervention schools with <10% missing data since 2009?
y8s  |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique, outcome_ISLED) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar, WPO_BRIN_unique, outcome_ISLED, fill = list(N_cat = 0)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  mutate(missing_perc = N_cat / sum(N_cat) * 100) |>
  filter(is.na(outcome_ISLED) & !is.nan(missing_perc)) |>
  filter(missing_perc < 10) |>
  ungroup() |>
  select(peiljaar) |>
  table()
# <10 schools with <10% missing data since 2009

###  CITO availability throughout the years ----
y8s_full  |>
  filter(intervention == "early") |>
  select(peiljaar, WPOBRIN_crypt, WPOBRINVEST, WPOCODEEINDTOETS) |>
  unique() |>
  View()
# CITO has become Centrale eindtoets since 2015
# DONE: check recoding of testtype variable => value "11" should also be CITO (next to value "01")
# for now checks with y8s_full
# continue checks with CITO here. use new recode.  
y8s_full <- 
  y8s_full |>
  mutate(
    outcome_CITO = ifelse(WPOCODEEINDTOETS == "01" | WPOCODEEINDTOETS == "11", WPOUITSLAGEINDTOETS, NA)
  )

y8s_full <- 
  y8s_full |>
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

table(y8s_full$peiljaar, y8s_full$WPOCODEEINDTOETS, useNA = "always")
# CITO usage drops after 2015
# 'Other' is very low in 2016. Probably because of shift to IEP and ROUTE8 and obligatory testing

# what percentage of students takes CITO test in intervention schools?
y8s_full  |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique, control_testtype) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar, WPO_BRIN_unique, control_testtype, fill = list(N_cat = 0)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  mutate(perc_CITO = N_cat / sum(N_cat) * 100) |>
  filter(control_testtype == "CITO" & !is.nan(perc_CITO)) |>
  select(peiljaar, WPO_BRIN_unique, perc_CITO) |>
  ungroup() |>
  ggplot(aes(x = peiljaar, y = perc_CITO)) +
  geom_point(position = position_jitter(width = .2, height = 0))
# in some schools the level of valid CITO scores is low. TODO: determine threshold for analysis

# How many intervention schools have >85% scores throughout the years?
y8s_full  |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique, control_testtype) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar, WPO_BRIN_unique, control_testtype, fill = list(N_cat = 0)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  mutate(perc_CITO = N_cat / sum(N_cat) * 100) |>
  filter(control_testtype == "CITO" & !is.nan(perc_CITO)) |>
  filter(perc_CITO > 85) |>
  ungroup() |>
  select(peiljaar) |>
  table()
# <10 schools in 2009, <10 schools in 2019. TODO: check whether these are the same schools across years

## average CITO scores for intervention groups throughout the years ----
# check distribution of CITO
summary(y8s_full$outcome_CITO)

y8s_full |>
  ggplot(aes(x = outcome_CITO)) +
  geom_boxplot()
# many values fall outside the range 500 - 550 => how many?
y8s_full |>
  select(outcome_CITO) |>
  mutate(insiderange_CITO =
           ifelse(outcome_CITO < 500 | outcome_CITO > 550,
                  0,
                  1)) |>
  select(insiderange_CITO) |>
  table()
# only 281 values => to be neglected. 
# TODO: remove values outside range CITO before analysis => DONE

# development in CITO scores for intervention groups
y8s_full  |>
  group_by(peiljaar, intervention) |>
  summarize(mean_CITO = mean(outcome_CITO, na.rm = T)) |>
  ungroup() |>
  filter(!is.nan(mean_CITO)) |>
  ggplot(aes(
    x = peiljaar,
    y = mean_CITO,
    group = intervention,
    colour = intervention
  )) +
  geom_line()
# Rise in CITO scores is likely because of selection in testtype between schools

# checks for control variables ----
## education, missings ----
y8s |>
  group_by(peiljaar, intervention, control_educ_missing_ma) |>
  summarize(N_cat = n()) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_educ_missing_ma == T) |>
  select(-N_cat, -control_educ_missing_ma) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = intervention,
    colour = intervention
  )) +
  geom_line()

y8s |>
  group_by(peiljaar, intervention, control_educ_missing_pa) |>
  summarize(N_cat = n()) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_educ_missing_pa == T) |>
  select(-N_cat, -control_educ_missing_pa) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = intervention,
    colour = intervention
  )) +
  geom_line()
# Coverage increases strongly after 2012. Coverage is higher for mothers than fathers
# TODO: decide on how to deal with this in analysis. 

## education, distributions for low and high ----
y8s |>
  group_by(peiljaar, intervention, control_educ_lower_ma) |>
  summarize(N_cat = n()) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_educ_lower_ma == T) |>
  select(-N_cat, -control_educ_lower_ma) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = intervention,
    colour = intervention
  )) +
  geom_line()
# TODO: check with EJ how to automate these plots for higher/lower - pa/ma combo's
# both higher and lower increase due to increased coverage. 
# TODO: consider how to aggregate education for analysis (see also previous paper Gijs). 
#       also check how educational level develops on intervention schools level 
#       when decision is taken. 

## descent, missing ----
table(y8s$control_desc_missing, useNA = "always")
# very low

##descent, demographic developments ----
# check percentage native children per school in intervention group
y8s |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique, control_desc_native) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar,
           WPO_BRIN_unique,
           control_desc_native,
           fill = list(N_cat = 0)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_desc_native == T & !is.nan(perc_cat)) |>
  select(-N_cat,-control_desc_native) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
  )) +
  geom_line() +
  guides(colour = F)
# some fluctuations per year
# TODO: automate generation plots for all ethnic groups

# development in intervention groups, native children
y8s |>
  group_by(peiljaar, intervention, control_desc_native) |>
  summarize(N_cat = n()) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_desc_native == T) |>
  select(-N_cat,-control_desc_native) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = intervention,
    colour = intervention
  )) +
  geom_line()
# steady decrease on the national level, whereas small increase since 2016
# TODO: automate generation plots for all ethnic groups

## income, wealth and social ----
# income, missings per year
summary(y8s$control_perc_income)

y8s |>
  select(peiljaar, control_perc_income) |>
  mutate(missing_income = 
           ifelse(
             is.na(control_perc_income),
             1,
             0
           )) |>
  select(-control_perc_income) |>
  group_by(peiljaar) |>
  summarize(missing_perc = mean(missing_income)) |> view()
# missings are concentrated in 2009 and 2010 (about 6% per year)

# income development in intervention schools
y8s |>
  filter(intervention == "early" & !is.na(control_perc_income)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  summarize(mean_income = mean(control_perc_income)) |>
  ggplot(aes(
    x = peiljaar,
    y = mean_income,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
  )) +
  geom_line() +
  guides(colour = F)

# wealth, missings per year
summary(y8s$control_perc_wealth)

y8s |>
  select(peiljaar, control_perc_wealth) |>
  mutate(missing_wealth = 
           ifelse(
             is.na(control_perc_wealth),
             1,
             0
           )) |>
  select(-control_perc_wealth) |>
  group_by(peiljaar) |>
  summarize(missing_perc = mean(missing_wealth)) |> View()
# 100% missing in 2009 and 2010 
# TODO: check whether data is missing or something went wrong

# income social, missings per year
summary(y8s$control_income_social)

y8s |>
  select(peiljaar, control_income_social) |>
  mutate(missing_social = 
           ifelse(
             is.na(control_income_social),
             1,
             0
           )) |>
  select(-control_income_social) |>
  group_by(peiljaar) |>
  summarize(missing_perc = mean(missing_social)) |> View()
# missings are concentrated in 2009 and 2010 (about 6% per year)

# social income development in intervention schools
y8s |>
  filter(intervention == "early" & !is.na(control_income_social)) |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  summarize(mean_social = mean(control_income_social)) |>
  ggplot(aes(
    x = peiljaar,
    y = mean_social,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
  )) +
  geom_line() +
  guides(colour = F)
# one school has 100% social in 2009 (cf. outlier with income) => 
# probably many missings (or few students)

## number of students ----
y8s |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  summarize(N_cat = n()) |>
  ungroup() |>
  complete(peiljaar,
           WPO_BRIN_unique,
           fill = list(N_cat = 0)) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = N_cat,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
    
  )) +
  geom_line() +
  guides(colour = F)
# some school have zero students, particularly in 2009 and 2014 => missing or 
# school did not exist?
# TODO: decide on how to deal with this.

## school stability ----
summary(y8s$control_schoolstability_trunc) # no missings

y8s |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  summarize(mean_stability = mean(control_schoolstability_trunc)) |>
  ggplot(aes(
    x = peiljaar,
    y = mean_stability,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
    
  )) +
  geom_line() +
  guides(colour = F)
# for most schools it seems pretty stable.

## class size ----
# calculate ratio between maximum class size and school N
y8s |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique) |>
  summarize(N_cat = n(),
            max_groupsize = max(control_groupsize)) |>
  mutate(ratio = max_groupsize / N_cat) |>
  filter(!is.na(ratio)) |>
  select(-N_cat,-max_groupsize) |>
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = ratio,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
  )) +
  geom_line() +
  guides(colour = F)
# For a few schools, the ratio goes above 1 for a couple of years.
# this might be due to unreliability WPOGROEPSGROOTTE, or missing students in data
# TODO: send email to CBS to check

## single parent hh ----
summary(y8s$control_single_parent) # a few missings, but neglible

y8s |>
  filter(intervention == "early") |>
  group_by(peiljaar, WPO_BRIN_unique, control_single_parent) |>
  summarize(N_cat = n()) |>
  mutate(perc_cat = N_cat / sum(N_cat) * 100) |>
  filter(control_single_parent == T) |>
  select(-N_cat, -control_single_parent) |> 
  ungroup() |>
  ggplot(aes(
    x = peiljaar,
    y = perc_cat,
    group = WPO_BRIN_unique,
    colour = WPO_BRIN_unique
  )) +
  geom_line() +
  guides(colour = F)
# quite some fluctuation between years
# TODO: maybe discuss whether this is useful as a pre-intervention variable

# general remarks
  # for the late intervention group there are strongly fluctuating scores 
  # on many variables around 2015-2017. If we are going to separately analysis 
  # this group - instead of using it as a control -  we need to delve into this


# find schools which are not in the data
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

