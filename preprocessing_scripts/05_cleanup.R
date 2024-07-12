# Post-process final year8_students file
# last edited 20230817 by @vankesteren

final_data_path <- file.path("scratch", "year8_students_peducation.rds")

# Copy file ----
cat(format(now()), "Copying year8 students file to processed_data directory.\n")
full_path <- file.path("processed_data", "year8_students_full.rds")
stopifnot(file.copy(final_data_path, full_path, overwrite = TRUE))


# Read data ----
cat(format(now()), "Reading in final year8 students file..\n")
y8s_full <- read_rds(full_path)

# Retaining only necessary columns ----
cat(format(now()), "Retaining and post-processing needed columns.\n")
y8s <- 
  y8s_full |> 
  select(
    RINPERSOON, RINPERSOONS,          # person identifier
    WPOBRIN_crypt, WPOBRINVEST,       # school identifier
    peiljaar,                         # which year
    intervention, intervention_hours, # intervention info
    starts_with("control_"),          # control variables
    starts_with("outcome_")           # outcome variables
  ) |> 
  mutate(
    # efficiently store these variables
    WPOBRIN_crypt = as_factor(WPOBRIN_crypt),
    WPOBRINVEST   = factor(WPOBRINVEST),
    peiljaar      = as_factor(peiljaar)
  )

# Write clean students file ----
cat(format(now()), "Writing cleaned students file.\n")
clean_path <- file.path("processed_data", "year8_students.rds")
write_rds(y8s, clean_path)
