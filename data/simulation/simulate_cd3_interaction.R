# Simulate CD3 data: y ~ (1 | pair_id) + ns(time) * male
# 10 pairs, each with a male and a female member
# time: integer 1 to 10
# The interaction ns(time):male gives males and females different trajectories
# Here the gap grows steadily over time

library(splines)
library(dplyr)
library(ggplot2)

set.seed(2026)

# -------------------------------
# 1. Set up the structure
# -------------------------------
n_pairs   <- 10
timepoints <- 1:10

pairs <- tibble(
  pair_id    = factor(rep(1:n_pairs, each = 2)),
  subject_id = factor(1:(2 * n_pairs)),
  male       = rep(c(0, 1), times = n_pairs)
)

dat <- pairs %>%
  slice(rep(1:n(), each = length(timepoints))) %>%
  mutate(time = rep(timepoints, times = nrow(pairs)))

# -------------------------------
# 2. Build the natural spline basis (df = 3)
# -------------------------------
ns_basis <- ns(timepoints, df = 3)
colnames(ns_basis) <- paste0("ns", 1:3)

cat("Natural spline basis (df = 3):\n")
print(round(cbind(as.data.frame(ns_basis), time = timepoints), 4))

# -------------------------------
# 3. Parameters
# -------------------------------
beta_0     <- 100       # intercept (female baseline at time = 1)

# Female trajectory coefficients (ns(time) main effect)
# Produces a rise-and-fall: low at t=1, peaks ~t=6-7, drops by t=10
beta_ns_female <- c(10, 8, -4)

# Constant male shift (keeps a small gap even at t=1)
beta_male <- 3

# Interaction coefficients: ns(time):male
# Carefully chosen so the additional male contribution is
# ~0 at t=1 and grows monotonically to ~11 at t=10
beta_ns_male <- c(0, 10, 10)

# Random intercept SD (between-pair variability)
sigma_pair   <- 10

# Residual SD
sigma_resid  <- 4

# -------------------------------
# 4. Compute the true trajectories (no noise)
# -------------------------------
time_effect_female <- as.vector(ns_basis %*% beta_ns_female)
time_effect_male   <- as.vector(ns_basis %*% beta_ns_male)  # additional for males

names(time_effect_female) <- timepoints
names(time_effect_male)   <- timepoints

cat("\n--- True trajectories (before random effects + noise) ---\n")
true_traj <- data.frame(
  time = timepoints,
  female = round(beta_0 + time_effect_female, 2),
  male   = round(beta_0 + time_effect_female + beta_male + time_effect_male, 2)
)
true_traj$gap <- round(true_traj$male - true_traj$female, 2)
print(true_traj)

# Random intercept per pair
pair_effects <- rnorm(n_pairs, mean = 0, sd = sigma_pair)
names(pair_effects) <- 1:n_pairs

dat <- dat %>%
  mutate(
    time_effect_f = time_effect_female[as.character(time)],
    time_effect_m = time_effect_male[as.character(time)],
    pair_effect   = pair_effects[as.character(pair_id)]
  )

# -------------------------------
# 5. Generate the response
# -------------------------------
dat <- dat %>%
  mutate(
    y = beta_0
      + time_effect_f           # ns(time)        — female trajectory
      + beta_male * male        # male            — constant shift
      + time_effect_m * male    # ns(time):male   — extra male trajectory
      + pair_effect             # random intercept
      + rnorm(n(), 0, sigma_resid),
    sex = ifelse(male == 1, "Male", "Female"),
    pair_label = paste("Pair", pair_id)
  )

# Observed gap (with noise)
obs_gap <- dat %>%
  group_by(time, sex) %>%
  summarise(y_mean = mean(y), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = y_mean) %>%
  mutate(obs_gap = Male - Female, true_gap = true_traj$gap)

cat("\n--- Observed gap (with noise) vs true gap ---\n")
print(round(obs_gap, 2))

# -------------------------------
# 6. Save data and parameters
# -------------------------------
write.csv(dat, "data/simulation/cd3_simulated_interaction.csv", row.names = FALSE)
cat("\nSaved data/simulation/cd3_simulated_interaction.csv  —", nrow(dat), "rows\n")

params <- list(
  formula = "y ~ (1 | pair_id) + ns(time) * male",
  n_pairs = n_pairs,
  timepoints = timepoints,
  beta_0 = beta_0,
  beta_ns_female = beta_ns_female,
  beta_male = beta_male,
  beta_ns_male = beta_ns_male,
  sigma_pair = sigma_pair,
  sigma_resid = sigma_resid,
  true_trajectories = true_traj
)
saveRDS(params, "data/simulation/parameters_interaction.rds")
cat("Saved data/simulation/parameters_interaction.rds\n")

# -------------------------------
# 7. Plot 1: All trajectories, faceted by pair
# -------------------------------
p1 <- ggplot(dat, aes(x = time, y = y, colour = sex, group = subject_id)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.7) +
  facet_wrap(~ pair_label, ncol = 5) +
  scale_colour_manual(values = c("Female" = "#E76F51", "Male" = "#264653")) +
  scale_x_continuous(breaks = timepoints) +
  labs(
    title = "Simulated CD3 trajectories — growing gap",
    subtitle = "y ~ (1 | pair_id) + ns(time) * male",
    x = "Time", y = "CD3", colour = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", colour = NA)
  )

ggsave("data/simulation/cd3_interaction_trajectories.png", p1,
       width = 10, height = 6, dpi = 150)
cat("Saved data/simulation/cd3_interaction_trajectories.png\n")

# -------------------------------
# 8. Plot 2: Mean trajectory by sex + growing gap annotation
# -------------------------------
means <- dat %>%
  group_by(time, sex) %>%
  summarise(y_mean = mean(y), .groups = "drop")

annotate_pts <- c(1, 5, 10)
gap_annot <- obs_gap %>%
  filter(time %in% annotate_pts)

p2 <- ggplot(means, aes(x = time, y = y_mean, colour = sex)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  # Vertical segments showing the gap at selected timepoints
  geom_segment(
    data = gap_annot,
    aes(x = time, xend = time, y = Female, yend = Male),
    colour = "grey50", linewidth = 0.8, linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = gap_annot,
    aes(x = time + 0.45,
        y = (Female + Male) / 2,
        label = paste0("+", round(true_gap, 1))),
    colour = "grey40", size = 3.5, hjust = 0,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = c("Female" = "#E76F51", "Male" = "#264653")) +
  scale_x_continuous(breaks = timepoints) +
  labs(
    title = "Mean CD3 trajectory by sex — the growing gap",
    subtitle = "The gap widens from ~3 at t=1 to ~14 at t=10",
    x = "Time", y = "Mean CD3", colour = "Sex"
  ) +
  theme_minimal(base_size = 11)

ggsave("data/simulation/cd3_interaction_mean_trajectories.png", p2,
       width = 7, height = 5, dpi = 150)
cat("Saved data/simulation/cd3_interaction_mean_trajectories.png\n")

print(p2)