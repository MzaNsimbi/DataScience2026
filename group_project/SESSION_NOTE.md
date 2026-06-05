# Session Note — Yogurt Data Dictionary

## What we did

Created a data dictionary for **Group A (Yogurt study)** — 4 CSV files in
`data/group_a_yogurt/`.

The output is a Quarto document at:

> **`group_project/yogurt_data_dictionary.qmd`**

Rendered HTML is at:

> `group_project/yogurt_data_dictionary.html`

Live preview (auto-reloads on save):

> https://bug-free-space-tribble-x5v6477w654wc69x7-4321.app.github.dev/

---

## The 4 files and what a row means

| File | Rows | One row = |
|------|------|-----------|
| `00_sample_ids_yogurt.csv` | 102 | One sample from one participant at one time point |
| `01_participant_metadata_yogurt.csv` | 51 | Background info for one participant |
| `02_qpcr_results_yogurt.csv` | 102 | Bacterial DNA measurements for one sample |
| `03_luminex_results_yogurt.csv` | 1020 | One cytokine concentration for one sample |

## Study design

- 51 female participants, two time points (baseline + after antibiotics)
- Two arms: **yogurt** vs. **unchanged_diet** (control)
- Measurements: bacterial DNA via qPCR, immune markers (cytokines) via Luminex

## How the files connect

- `pid` links `00_sample_ids` ↔ `01_participant_metadata`
- `sample_id` links `00_sample_ids` ↔ `02_qpcr_results` ↔ `03_luminex_results`

## Key R code used

```r
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Read files
samples  <- read_csv("data/group_a_yogurt/00_sample_ids_yogurt.csv")
metadata <- read_csv("data/group_a_yogurt/01_participant_metadata_yogurt.csv")
qpcr     <- read_csv("data/group_a_yogurt/02_qpcr_results_yogurt.csv")
luminex  <- read_csv("data/group_a_yogurt/03_luminex_results_yogurt.csv")

# Explore
glimpse(samples)        # column names, types, preview values
nrow(samples)           # row count
samples |> head()       # first 6 rows
samples |> distinct(arm)  # unique values in a column
samples |> count(time_point)  # rows per group (shorthand for group_by + summarise)
samples |> summarise(n = n_distinct(pid))  # count unique values
```

## How to re-launch the preview

```bash
cd /workspaces/DataScience2026
PORT=4321
setsid quarto preview group_project/yogurt_data_dictionary.qmd \
  --no-browser --port $PORT > /tmp/quarto-preview.log 2>&1 < /dev/null &

URL="https://${CODESPACE_NAME}-${PORT}.${GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN}/"
printf '\n\e]8;;%s\a▶ Open Quarto preview\e]8;;\a\n' "$URL"
```
