library(tidyverse)
library(purrr)
library(patchwork)

# read data and synth weights
psynth_list <- read_rds("processed_data/psynth_list_cito.rds")
synth_mats <- read_rds("processed_data/synth_mats_cito.rds")


# Function to plot the synthetic control based on unit weights
synth_tibble <- function(matrices, weights) {
  out1 <- rbind(matrices$Z1, matrices$Y1)
  out0 <- rbind(matrices$Z0, matrices$Y0) 
  sync <- out0 %*% c(weights)
  yr <- parse_number(rownames(out1))
  
  tibble(
    outcome = c(out1, sync), 
    mode    = rep(c("Original data", "Synthetic control"), each = length(sync)),
    yr    = rep(yr, 2)
  )
}

plot_synth <- function(matrices, weights) {
  df <- synth_tibble(matrices, weights)
  df |> 
    ggplot(aes(x = yr, y = outcome, linetype = mode, pch = mode)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 2013.5, linetype = 4) +
    scale_x_continuous(breaks = unique(df$yr)) +
    theme_minimal() +
    labs(
      x = "", 
      y = "Outcome",
      pch = "", 
      linetype = ""
    ) +
    theme(legend.position = "top", legend.justification = "left")
}

# example usage:
plot_synth(synth_mats[[3]], psynth_list[[3]]$w_opt)


# do all of them
plot_list <- imap(synth_mats, \(m, i) plot_synth(m, psynth_list[[i]]$w_opt))
iwalk(plot_list, \(p, i) ggsave(
  plot = p + labs(title = i), 
  filename = paste0("img/synplots/trend_", i, "_cito.png"), 
  width = 9, height = 7, bg = "white"
), .progress = TRUE)

# all at once 
wrap_plots(plot_list, guides = "collect", nrow = 5, ncol = 5)
ggsave("img/synth_plots_cito.png", width = 12, height = 7)


# create a balance table
balance_table <- 
  bind_cols(
    map(
      .x = seq_along(synth_mats),
      .f = function(i) {
        bt <- cbind(
          rbind(synth_mats[[i]]$X1, synth_mats[[i]]$Z1), 
          rbind(synth_mats[[i]]$X0, synth_mats[[i]]$Z0) %*% 
            c(psynth_list[[i]]$w_opt)
        )
        colnames(bt) <- c(
          colnames(bt)[1], 
          paste0("synthetic_", colnames(bt)[1])
        )
        return(bt)
      }
    )
  ) |> 
  mutate(variable = rownames(rbind(synth_mats[[1]]$X1, synth_mats[[1]]$Z1))) |> 
  pivot_longer(-variable, names_to = "school_id") |>
  mutate(
    synthetic = factor(
      str_detect(school_id, "synthetic"),
      levels = c(TRUE, FALSE),
      labels = c("synthetic", "real")
    ),
    school_id = str_remove(school_id, "synthetic_")
  ) |> 
  mutate(variable = as_factor(variable))


# balance by variable
balance_table |> 
  group_by(variable) |> 
  mutate(value = c(scale(value))) |> 
  ungroup() |> 
  ggplot(aes(x = synthetic, y = value, group = school_id)) +
  geom_line(colour = "grey") +
  geom_point(aes(fill = synthetic), colour = "white", pch = 21, size = 1.9) +
  facet_wrap(vars(variable), scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "black"), guide = "none") +
  labs(x = "", y = "", title = "Covariate balance for synthetic controls")


# balance by unit
balance_table |> 
  group_by(variable) |> 
  mutate(value = c(scale(value))) |> 
  ungroup() |> 
  ggplot(aes(x = synthetic, y = value, group = variable)) +
  geom_line(colour = "grey") +
  geom_point(aes(fill = synthetic), colour = "white", pch = 21, size = 1.9) +
  facet_wrap(vars(school_id), scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "black"), guide = "none") +
  labs(x = "", y = "", title = "Covariate balance for synthetic controls")


# summarized version for disclosure control
balance_table |> 
  summarize(
    est = mean(value, na.rm = TRUE),
    sdev = sd(value, na.rm = TRUE),
    lower = pmax(0, est - 1.96*sdev),
    upper = est + 1.96*sdev,
    .by = c(variable, synthetic)
  ) |> 
  ggplot(aes(x = synthetic, y = est, group = variable)) +
  geom_line(colour = "grey") +
  geom_pointrange(
    mapping = aes(
      fill = synthetic, 
      ymin = lower, 
      ymax = upper
    )
  ) +
  facet_wrap(vars(as_factor(variable)), scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "black"), guide = "none") +
  labs(x = "", y = "", title = "Covariate balance for synthetic control")

ggsave("img/balance_plot_nondisclosive_cito.png", width = 11, height = 7, bg = "white")

# balance delta
balance_table |> 
  pivot_wider(
    names_from = synthetic,
    values_from = value
  ) |> 
  mutate(
    delta = real - synthetic,
    smd   = delta / sd(delta, na.rm = TRUE),
    .by   = variable
  ) |> 
  summarize(
    est   = mean(smd, na.rm = TRUE),
    sdev  = sd(smd, na.rm = TRUE),
    lower = est - 1.96*sdev/sqrt(n()),
    upper = est + 1.96*sdev/sqrt(n()),
    .by   = variable
  ) |> 
  ggplot(
    aes(
      x      = est,
      y      = fct_rev(variable),
      xmin   = lower,
      xmax   = upper,
      colour = str_detect(variable, "outcome")
    )
  ) + 
  geom_segment(aes(x = est - 2*sdev, xend = est + 2*sdev, yend = fct_rev(variable))) + 
  geom_pointrange(linewidth = 1.2) +
  scale_colour_manual(values = c("black", "seagreen"), guide = "none") +
  xlim(-4, 4) +
  theme_minimal() +
  labs(
    title    = "Covariate balance (standardized mean differences)",
    subtitle = "Real school value - synthetic school value",
    x        = "Standardized mean difference\n95% CI, ± 2SD",
    y        = ""
  )

ggsave("img/balance_plot_smd_cito.png", width = 7, height = 7, bg = "white")
  

# Create aggregate synthetic control plot
tib_list <- imap(synth_mats, \(m, i) synth_tibble(m, psynth_list[[i]]$w_opt))
synth_control_df <- bind_rows(tib_list, .id = "school_id")

synth_control_summary <- 
  synth_control_df |> 
  summarize(
    variance = var(outcome),
    n = n(),
    outcome = mean(outcome), 
    .by = c(yr, mode)
  )

# Full data plot
synth_control_df |> 
  ggplot(aes(x = yr, y = outcome, colour = mode)) +
  geom_line(aes(group = paste(school_id, mode)), alpha = 0.2) +
  geom_line(data = synth_control_summary, linewidth = 1.5) +
  geom_vline(xintercept = 2013.5, linetype = 2, colour = "#343434") +
  theme_minimal() +
  scale_x_continuous(breaks = 2009:2022) +
  scale_colour_manual(values = c("steelblue", "orangered2")) +
  labs(
    title = "Real and estimated synthetic school performance",
    subtitle = "Individual schools and average trajectory",
    x = "Year", 
    y = "School performance (CITO score)", 
    colour = "", fill = ""
  ) +
  ylim(500, 550) +
  theme(legend.position = "top", legend.justification = "right")

ggsave("img/synthetic_control_plot_cito.png", width = 12, height = 7, bg = "white")

# Now the same but without full data for disclosure control
synth_control_summary |> 
  ggplot(aes(x = yr, y = outcome, colour = mode)) +
  geom_ribbon(
    mapping = aes(
      fill = mode,
      ymin = outcome - 1.96 * sqrt(variance),
      ymax = outcome + 1.96 * sqrt(variance)
    ),
    colour = NA,
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2013.5, linetype = 2, colour = "#343434") +
  theme_minimal() +
  scale_x_continuous(breaks = 2009:2022) +
  scale_colour_manual(values = c("steelblue", "orangered2")) +
  scale_fill_manual(values = c("steelblue", "orangered2")) +
  labs(
    title = "Real and estimated synthetic school performance",
    subtitle = "Average trajectory and school data distribution",
    x = "Year", 
    y = "School performance (CITO score)", 
    colour = "", fill = ""
  ) +
  ylim(500, 550) + 
  theme(legend.position = "top", legend.justification = "right")

ggsave("img/synthetic_control_plot_data_dist_cito.png", width = 12, height = 7, bg = "white")

# Now once more but with standard error instead of standard deviation
synth_control_summary |> 
  ggplot(aes(x = yr, y = outcome, colour = mode)) +
  geom_ribbon(
    mapping = aes(
      fill = mode,
      ymin = outcome - 1.96 * sqrt(variance) / sqrt(n),
      ymax = outcome + 1.96 * sqrt(variance) / sqrt(n)
    ),
    colour = NA,
    alpha = 0.2
  ) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2013.5, linetype = 2, colour = "#343434") +
  theme_minimal() +
  scale_x_continuous(breaks = 2009:2022) +
  scale_colour_manual(values = c("steelblue", "orangered2")) +
  scale_fill_manual(values = c("steelblue", "orangered2")) +
  labs(
    title = "Real and estimated synthetic school performance",
    subtitle = "Average trajectory and Wald-type 95% confidence interval",
    x = "Year", 
    y = "School performance (CITO score)", 
    colour = "", fill = ""
  ) +
  ylim(500, 550) +
  theme(legend.position = "top", legend.justification = "right")

ggsave("img/synthetic_control_plot_confidence_cito.png", width = 12, height = 7, bg = "white")

# Compute average causal effect in post-treatment time-series
compute_ace <- function(m, i) {
  m |> 
    synth_tibble(psynth_list[[i]]$w_opt) |> 
    pivot_wider(names_from = mode, values_from = outcome) |> 
    mutate(diff = `Original data` - `Synthetic control`) |> 
    filter(yr >= 2014) |> 
    pull(diff) |> 
    mean()
}
aces <- imap_dbl(synth_mats, compute_ace, .progress = TRUE)

mean(aces)
sd(aces) / sqrt(length(aces))
t.test(aces)

set.seed(45)
ggplot(
  tibble(ace = aces), aes(x = ace)) +
  geom_density(fill = "darkorange") +
  #geom_point(y = rnorm(length(aces), -0.01, 0.001), pch = 21, fill = "steelblue", colour = "white", size = 3) +
  xlim(-15, 15) +
  # ylim(-0.04, 0.17) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  labs(
    title = "Average causal effect post-intervention",
    subtitle = "Averaged over years 2014 - 2022",
    # y = "",
    x = "ACE (improvement in CITO score)"
  ) #+
  # theme(axis.text.y = element_blank())

ggsave("img/ace_cito.png", width = 10, height = 4.5, bg = "white")
