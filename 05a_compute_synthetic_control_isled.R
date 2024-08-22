# Script to compute synthetic control weights
# i.e., X1, X0, Z1, Z0, and v weights for each school.
# last edited 2024-01-26 by @vankesteren
library(tidyverse)
library(furrr)
plan(multisession)
library(pensynth) # github.com/vankesteren/pensynth

# read the prepared data
synth_mats <- read_rds("processed_data/synth_mats_isled.rds")

# Compute penalized synthetic control
# use Z (pre-intervention outcomes) as cross-validation target to tune lambda
# and equally weigh pre-intervention covariates on a standardized scale
psynth_list <- future_map(
  .x = synth_mats, 
  .f = function(m) {
    cv_pensynth(
      X1 = m$X1,
      X0 = m$X0,
      v  = 1,
      Z1 = m$Z1,
      Z0 = m$Z0,
      nlambda = 100
    )
  }, 
  .progress = TRUE
)
write_rds(psynth_list, "processed_data/psynth_list_isled.rds")

# Showcase: weight path
png("img/weight_path_school_1.png", width = 2400, height = 1800, res = 200)
plot_path(psynth_list[[1]])
dev.off()

png("img/weights_school_1.png", width = 2400, height = 1800, res = 200)
par(mfrow = c(2, 1))
barplot(psynth_list[[1]]$w_path[,1], main = "Unpenalized weights")
barplot(psynth_list[[1]]$w_opt, main = "CV penalized weights")
dev.off()

# Another showcase: penalization helps with simplification
school_id <- names(synth_mats)[12]
synth_orig <- with(
  synth_mats[[school_id]], 
  Synth::synth(X1 = X1, X0 = X0, custom.v = rep(1, 11), Z1 = Z1, Z0 = Z0)
)
res_no_pen <- with(synth_mats[[school_id]], pensynth(X1, X0, 1, lambda = 0))
res_pen <- with(synth_mats[[school_id]], pensynth(X1, X0, 1, lambda = 0.1))

tibble(
  weights = c(synth_orig$solution.w, res_no_pen$w, res_pen$w),
  donor   = as_factor(rep(colnames(synth_mats[[school_id]]$X0), 3)), 
  method  = as_factor(
    rep(c("Synth", "Pensynth, lambda = 0", "Pensynth, lambda = 0.1"), 
        each = length(res_pen$w))
  )
) |> 
  ggplot(aes(y = weights, x = as.numeric(donor), fill = method)) +
  geom_col(position = position_dodge()) + 
  scale_fill_brewer(type = "qual", guide = "none") +
  theme_linedraw() +
  facet_grid(rows = vars(method)) +
  labs(
    title = "Unit weight comparison from unpenalized and penalized synthetic controls",
    y = "Unit weight", 
    x = "Donor unit"
  )

ggsave("img/penalized_showcase.png", width = 9, height = 8, dpi = "retina")
