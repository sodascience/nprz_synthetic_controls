# aggregating file from student-level to school-level
library(tidyverse)
y8s <- read_rds("processed_data/year8_students.rds")

# Helper functions for the summarization step
summary_fns <- list(
  mean    = function(x) suppressWarnings(mean(x, na.rm = TRUE)), 
  valid   = function(x) sum(!is.na(x)),
  missing = function(x) sum(is.na(x)),
  se      = function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid < 2) return(NA)
    return(sd(x, na.rm = TRUE) / sqrt(n_valid))
  }
)

get_most_common <- function(x) names(sort(table(x), decreasing = TRUE)[1])
get_first <- function(x) first(x, na_rm = TRUE)

# actually do the summarization (this takes a while)
school_data <- 
  y8s |> 
  mutate(school_id = paste(WPOBRIN_crypt, WPOBRINVEST, sep = "_")) |> 
  summarise(
    
    # for person characteristics, use the mean
    across(
      .cols  = c(starts_with("outcome"), starts_with("control"), -starts_with("control_school"), -control_testtype),
      .fns   = summary_fns, 
      .names = "{.col}_{.fn}"
    ),
    
    # for school characteristics, get the first value
    across(c(starts_with("control_school"), starts_with("intervention")), get_first),
    
    # for testtype, get the most common value
    control_testtype_mostcommon = get_most_common(control_testtype),
    
    .by = c("school_id", "peiljaar")
  ) |> 
  mutate(
    across(c(starts_with("control_school"), control_testtype_mostcommon), as.factor)
  )

# clean up and store
school_data <- 
  school_data |> 
  select(
    school_id, peiljaar, intervention, intervention_hours, 
    starts_with("control"), starts_with("outcome")
  )

write_rds(school_data, "processed_data/school_data.rds")
