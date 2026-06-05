# ──────────────────────────────────────────────────────────
# Box plot: qPCR bacteria — yogurt vs unchanged_diet
# At baseline and after antibiotic, with p-values
# ──────────────────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(rstatix)

# ── 1. Read data ─────────────────────────────────────────
meta    <- read.csv("data/group_a_yogurt/01_participant_metadata_yogurt.csv")
samples <- read.csv("data/group_a_yogurt/00_sample_ids_yogurt.csv")
qpcr    <- read.csv("data/group_a_yogurt/02_qpcr_results_yogurt.csv")

# ── 2. Merge ──────────────────────────────────────────────
# Join qPCR values with time_point and arm info
df <- samples %>%
  left_join(qpcr, by = "sample_id") %>%
  # Clean up arm labels for nicer plots
  mutate(arm = recode(arm,
                      "unchanged_diet" = "Unchanged diet",
                      "yogurt"         = "Yogurt"),
         time_point = recode(time_point,
                             "baseline"        = "Baseline",
                             "after_antibiotic" = "After antibiotic"))

# ── 3. Pivot to long format ──────────────────────────────
df_long <- df %>%
  pivot_longer(
    cols = c(qpcr_bacteria, qpcr_crispatus, qpcr_iners),
    names_to  = "bacteria",
    values_to = "value"
  ) %>%
  mutate(
    bacteria = recode(bacteria,
                      "qpcr_bacteria" = "Total bacteria",
                      "qpcr_crispatus" = "L. crispatus",
                      "qpcr_iners"    = "L. iners")
  )

# ── 4. Compute p-values (t-test + Wilcoxon) per panel ────
pvals <- df_long %>%
  group_by(time_point, bacteria) %>%
  summarise(
    t_pval    = tryCatch(t.test(value ~ arm)$p.value,
                         error = function(e) NA),
    wilcox_pval = tryCatch(wilcox.test(value ~ arm)$p.value,
                           error = function(e) NA),
    .groups = "drop"
  ) %>%
  mutate(
    t_label    = paste0("t-test p = ", formatC(t_pval, format = "e", digits = 2)),
    wilcox_label = paste0("Wilcoxon p = ", formatC(wilcox_pval, format = "e", digits = 2)),
    combined_label = paste0(t_label, "\n", wilcox_label)
  )

# ── 5. Build plot ────────────────────────────────────────
# For each time_point × bacteria panel we want a single comparison
# (yogurt vs unchanged diet). We'll use stat_compare_means with
# custom annotations for both tests.

# Manually annotate using geom_text for full control
p <- df_long %>%
  ggplot(aes(x = arm, y = value, fill = arm)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "grey30") +
  facet_grid(bacteria ~ time_point, scales = "free_y") +
  scale_fill_manual(values = c("Unchanged diet" = "#4A90D9", "Yogurt" = "#E8833A")) +
  labs(
    title    = "qPCR Bacteria — Yogurt vs Unchanged Diet Arm",
    subtitle = "Comparison at baseline and after antibiotic",
    x        = NULL,
    y        = "Bacterial load (copies/mL)",
    fill     = "Arm"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom"
  )

# ── 6. Add p-value text annotations ──────────────────────
pvals_annot <- pvals %>%
  mutate(
    # Place text in top-right of each panel
    x_pos = 2.3,
    y_pos = df_long %>%
      group_by(time_point, bacteria) %>%
      summarise(max_val = max(value, na.rm = TRUE) * 1.15, .groups = "drop") %>%
      pull(max_val)
  )

# Merge annotation positions
pvals_annot <- df_long %>%
  group_by(time_point, bacteria) %>%
  summarise(y_max = max(value, na.rm = TRUE) * 1.2, .groups = "drop") %>%
  left_join(pvals, by = c("time_point", "bacteria"))

p <- p +
  geom_text(
    data = pvals_annot,
    aes(x = 2.3, y = y_max, label = combined_label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 1, size = 3.1, color = "grey30"
  )

# ── 7. Save ──────────────────────────────────────────────
ggsave("notebooks/qpcr_boxplot_arms.png", p, width = 10, height = 10, dpi = 200)
print("Plot saved → notebooks/qpcr_boxplot_arms.png")
print(p)
