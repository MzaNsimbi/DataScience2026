# Simulate CD3 data: y ~ ns(time) + (1 | pair_id) + male
# 10 pairs, each with a male and a female member
# time: integer 1 to 10

library(splines)
library(dplyr)
library(ggplot2)

set.seed(2026)

# -------------------------------
# 1. Set up the structure
# -------------------------------
n_pairs   <- 10       # 10 pairs = 20 subjects
timepoints <- 1:10    # measured at times 1-10

# Each pair has one male and one female subject
pairs <- tibble(
  pair_id = factor(rep(1:n_pairs, each = 2)),
  subject_id = factor(1:(2 * n_pairs)),
  male = rep(c(0, 1), times = n_pairs)  # 0 = female, 1 = male
)

# Expand to all timepoints
dat <- pairs %>%
  slice(rep(1:n(), each = length(timepoints))) %>%
  mutate(time = rep(timepoints, times = nrow(pairs)))

# -------------------------------
# 2. Parameters for the simulation
# -------------------------------
# Fixed intercept
beta_0 <- 100

# Natural spline for time (df = 3 → 3 basis columns → 3 coefficients)
# We define a smooth trajectory: rises from time 1-4, plateaus around time 5-7,
# then a slight dip
ns_basis <- ns(timepoints, df = 3)
# Spline coefficients (chosen to produce a sensible non-linear CD3 trajectory)
beta_ns <- c(15, 8, -5)  # ~rising then plateauing then slight dip

# Male effect: males have 12 units higher CD3 on average
beta_male <- 12

# Random intercept SD: between-pair variability
sigma_pair <- 10

# Residual SD
sigma_resid <- 4

# -------------------------------
# 3. Generate the linear predictor
# -------------------------------
# Spline component: for each timepoint, compute ns %*% beta_ns
time_effect <- as.vector(ns_basis %*% beta_ns)
names(time_effect) <- timepoints

# Add the time effect to the data
dat <- dat %>%
  mutate(time_effect = time_effect[as.character(time)])

# Random intercept per pair
pair_effects <- rnorm(n_pairs, mean = 0, sd = sigma_pair)
names(pair_effects) <- 1:n_pairs

dat <- dat %>%
  mutate(pair_effect = pair_effects[as.character(pair_id)])

# -------------------------------
# 4. Generate the response
# -------------------------------
dat <- dat %>%
  mutate(
    y = beta_0 + time_effect + beta_male * male + pair_effect + rnorm(n(), 0, sigma_resid),
    sex = ifelse(male == 1, "Male", "Female"),
    pair_label = paste("Pair", pair_id)
  )

# -------------------------------
# 5. Save the data
# -------------------------------
write.csv(dat, "data/simulation/cd3_simulated.csv", row.names = FALSE)
cat("Saved data/simulation/cd3_simulated.csv  —", nrow(dat), "rows\n")

# Also save the parameters for reference
params <- list(
  n_pairs = n_pairs,
  timepoints = timepoints,
  beta_0 = beta_0,
  beta_ns = beta_ns,
  beta_male = beta_male,
  sigma_pair = sigma_pair,
  sigma_resid = sigma_resid
)
saveRDS(params, "data/simulation/parameters.rds")
cat("Saved data/simulation/parameters.rds\n")

# -------------------------------
# 6. Plot: all trajectories, coloured by sex, faceted by pair
# -------------------------------
p <- ggplot(dat, aes(x = time, y = y, colour = sex, group = subject_id)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ pair_label, ncol = 5) +
  scale_colour_manual(values = c("Female" = "#E76F51", "Male" = "#264653")) +
  scale_x_continuous(breaks = timepoints) +
  labs(
    title = "Simulated CD3 trajectories",
    subtitle = "y ~ ns(time) + (1 | pair_id) + male",
    x = "Time",
    y = "CD3",
    colour = "Sex"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", colour = NA)
  )

ggsave("data/simulation/cd3_trajectories.png", p, width = 10, height = 6, dpi = 150)
cat("Saved data/simulation/cd3_trajectories.png\n")

# -------------------------------
# 7. Additional plot: mean trajectory per sex (show gap)
# -------------------------------
# Compute mean by sex at each timepoint (averaging over pairs)
means <- dat %>%
  group_by(time, sex) %>%
  summarise(y_mean = mean(y), .groups = "drop")

p2 <- ggplot(means, aes(x = time, y = y_mean, colour = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Annotate the gap at one timepoint
  annotate("segment",
    x = 7, xend = 7,
    y = means$y_mean[means$time == 7 & means$sex == "Female"],
    yend = means$y_mean[means$time == 7 & means$sex == "Male"],
    colour = "grey50", linewidth = 0.6, linetype = "dashed"
  ) +
  annotate("text",
    x = 7.6, y = mean(means$y_mean[means$time == 7]),
    label = sprintf("Male effect = +%d", beta_male),
    size = 3.5, colour = "grey40", hjust = 0
  ) +
  scale_colour_manual(values = c("Female" = "#E76F51", "Male" = "#264653")) +
  scale_x_continuous(breaks = timepoints) +
  labs(
    title = "Mean CD3 trajectory by sex",
    subtitle = sprintf("Showing the male–female gap (beta_male = +%d)", beta_male),
    x = "Time",
    y = "Mean CD3",
    colour = "Sex"
  ) +
  ylim(floor(min(means$y_mean) / 10) * 10, ceiling(max(means$y_mean) / 10) * 10) +
  theme_minimal(base_size = 11)

ggsave("data/simulation/cd3_mean_trajectories.png", p2, width = 7, height = 5, dpi = 150)
cat("Saved data/simulation/cd3_mean_trajectories.png\n")

# Preview
print(p2)
