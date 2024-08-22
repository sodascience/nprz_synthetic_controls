library(tidyverse)
library(purrr)
library(writexl)

# function to create an output package for an analysis
write_analysis_output <- function(input_psynth_list = "processed_data/psynth_list_isled.rds",
                                  input_synth_mats = "processed_data/synth_mats_isled.rds",
                                  output_xlsx = "output/aug 2024/main_analysis_output.xlsx") {
  
  # read data and synth weights
  psynth_list <- read_rds(input_psynth_list)
  synth_mats <- read_rds(input_synth_mats)
  
  # create output list
  out_list <- list()
  
  # Number of non-zero weights
  # zero considered as weight below 0.005
  nonzero_weights <- map_dbl(psynth_list, \(x) sum(x$w_opt > 5e-3))
  out_list$nonzero_weights <- tibble(
    mean = mean(nonzero_weights),
    sd = sd(nonzero_weights),
    n = length(nonzero_weights)
  )
  
  # Balance plot
  # create a balance table
  smd_plot_data <-
    bind_cols(map(
      .x = seq_along(synth_mats),
      .f = function(i) {
        bt <- cbind(
          rbind(synth_mats[[i]]$X1, synth_mats[[i]]$Z1),
          rbind(synth_mats[[i]]$X0, synth_mats[[i]]$Z0) %*% c(psynth_list[[i]]$w_opt),
          rbind(synth_mats[[i]]$X0, synth_mats[[i]]$Z0) %*% rep(1 / ncol(synth_mats[[i]]$X0), ncol(synth_mats[[i]]$X0))
        )
        colnames(bt) <- c(
          paste0("real_", colnames(bt)[1]),
          paste0("synthetic_", colnames(bt)[1]),
          paste0("equalweight_", colnames(bt)[1])
        )
        return(bt)
      }
    )) |>
    mutate(variable = rownames(rbind(synth_mats[[1]]$X1, synth_mats[[1]]$Z1))) |>
    pivot_longer(
      -variable,
      names_to = c("synthetic", "school_id"),
      names_pattern = "^(\\w+)_(.+_\\d{2})$"
    ) |>
    mutate(synthetic = as_factor(synthetic),
           variable = as_factor(variable))  |>
    pivot_wider(names_from = synthetic, values_from = value) |>
    mutate(
      delta_synthetic = real - synthetic,
      smd_synthetic   = delta_synthetic / sd(delta_synthetic, na.rm = TRUE),
      delta_equalweight = real - equalweight,
      smd_equalweight   = delta_equalweight / sd(delta_equalweight, na.rm = TRUE),
      .by   = variable
    ) |>
    summarize(
      smd_synthetic = mean(smd_synthetic, na.rm = TRUE),
      smd_equalweight = mean(smd_equalweight, na.rm = TRUE),
      n     = n(),
      .by   = variable
    )
  
  out_list$covariate_balance_smd <- smd_plot_data
  
  
  # output timeseries
  
  # Create aggregate synthetic control plot
  synth_tibble <- function(matrices, weights) {
    out1 <- rbind(matrices$Z1, matrices$Y1)
    out0 <- rbind(matrices$Z0, matrices$Y0)
    sync <- out0 %*% c(weights)
    yr <- parse_number(rownames(out1))
    
    tibble(
      outcome = c(out1, sync),
      mode    = rep(c("Original data", "Synthetic control"), each = length(sync)),
      year    = rep(yr, 2)
    )
  }
  tib_list <- imap(synth_mats, \(m, i) synth_tibble(m, psynth_list[[i]]$w_opt))
  synth_control_df <- bind_rows(tib_list, .id = "school_id")
  
  synth_control_summary <-
    synth_control_df |>
    summarize(
      n = n(),
      variance = var(outcome),
      outcome = mean(outcome),
      .by = c(year, mode)
    )
  
  out_list$synth_control_timeseries <- synth_control_summary
  
  # Create average treatment effect on the treated density
  # Compute average causal effect in post-treatment time-series
  compute_ace <- function(m, i) {
    m |>
      synth_tibble(psynth_list[[i]]$w_opt) |>
      pivot_wider(names_from = mode, values_from = outcome) |>
      mutate(diff = `Original data` - `Synthetic control`) |>
      filter(year >= 2014) |>
      pull(diff) |>
      mean()
  }
  aces <- imap_dbl(synth_mats, compute_ace, .progress = TRUE)
  out_list$average_causal_effects <- tibble(ace = aces, n = 9)
  
  # write everything to excel worksheets
  write_xlsx(out_list, output_xlsx)
}

# write main analysis output
write_analysis_output(
  input_psynth_list = "processed_data/psynth_list_isled.rds",
  input_synth_mats = "processed_data/synth_mats_isled.rds",
  output_xlsx = "output/aug 2024/analysis_output_isled.xlsx"
)

# write cito analysis output
write_analysis_output(
  input_psynth_list = "processed_data/psynth_list_cito.rds",
  input_synth_mats = "processed_data/synth_mats_cito.rds",
  output_xlsx = "output/aug 2024/analysis_output_cito.xlsx"
)

# write main robustness output
write_analysis_output(
  input_psynth_list = "processed_data/psynth_list_robustness_isled.rds",
  input_synth_mats = "processed_data/synth_mats_robustness_isled.rds",
  output_xlsx = "output/aug 2024/analysis_output_robustness_isled.xlsx"
)

# write cito robustness output
write_analysis_output(
  input_psynth_list = "processed_data/psynth_list_robustness_cito.rds",
  input_synth_mats = "processed_data/synth_mats_robustness_cito.rds",
  output_xlsx = "output/aug 2024/analysis_output_robustness_cito.xlsx"
)


# Descriptive summary table
df <- read_rds("processed_data/school_data.rds")
df_descriptive <- 
  df |> 
  mutate(prepost = if_else(as.integer(as.character(peiljaar)) < 2014, "pre", "post")) |> 
  select(intervention, prepost, school_id, ends_with("mean")) |> 
  rename_with(ends_with("mean"), .fn = \(x) str_remove(x, "_mean")) |> 
  summarize(
    .by = c(intervention, prepost),
    across(matches("control.*"), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))),
    across(matches("outcome.*"), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))),
    sample_size = length(unique(school_id))
  ) |> 
  relocate(intervention, prepost, sample_size) |> 
  pivot_longer(
    cols = 4:last_col(), 
    names_pattern = "^(\\w[^_]+)_(.*)_(\\w+)$",
    names_to = c("var_type", "name", "fn")
  ) |> 
  mutate(name = replace_na(name, "sample_size"), fn = replace_na(fn, "mean")) |> 
  pivot_wider(
    names_from = fn,
    values_from = value
  ) |> 
  filter(
    intervention != "late", 
    !name %in% c("HAVOVWO", "KADER", "VMBO")
  ) |> 
  relocate(n = sample_size, .after = everything())

write_xlsx(df_descriptive, path = "output/aug 2024/descriptive_table.xlsx")

# time-series of demographics / control variables
df <- read_rds("processed_data/school_data.rds")
df_descriptive_timeseries <- df |> 
  select(intervention, school_id, peiljaar, ends_with("mean")) |> 
  rename_with(ends_with("mean"), .fn = \(x) str_remove(x, "_mean")) |> 
  summarize(
    .by = c(intervention, peiljaar),
    across(matches("control.*"), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))),
    sample_size = n()
  ) |> 
  relocate(intervention, peiljaar, sample_size) |> 
  pivot_longer(
    cols = 4:last_col(), 
    names_pattern = "^(\\w[^_]+)_(.*)_(\\w+)$",
    names_to = c("var_type", "name", "fn")
  ) |> 
  mutate(name = replace_na(name, "sample_size"), fn = replace_na(fn, "mean")) |> 
  pivot_wider(
    names_from = fn,
    values_from = value
  ) |> 
  filter(intervention != "late") |> 
  relocate(n = sample_size, .after = everything())


write_xlsx(df_descriptive_timeseries, path = "output/aug 2024/descriptive_timeseries.xlsx")
