
## Challenge: #SWDchallenge 2026 -- July
## Topic:     have a ball: visualize the World Cup
## Author:    Steven Ponce
## Date:      2026-07-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   Carrier On-Time Performance Dataset
##   https://www.kaggle.com/datasets/mexwell/carrier-on-time-performance-dataset


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, scales, glue, patchwork
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 5,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
## Source: Fjelstul World Cup Database (Joshua C. Fjelstul, Ph.D.)
## https://github.com/jfjelstul/worldcup -- CC-BY-SA 4.0
## Proximate access: cloned from GitHub master branch, data-csv/ folder.
tournaments     <- read_csv(here::here("2026/07_Jul/tournaments.csv"), show_col_types = FALSE) |> clean_names()
host_countries  <- read_csv(here::here("2026/07_Jul/host_countries.csv"), show_col_types = FALSE) |> clean_names()
qualified_teams <- read_csv(here::here("2026/07_Jul/qualified_teams.csv"), show_col_types = FALSE) |> clean_names()


## 3. EXAMINE THE DATA ----
glimpse(tournaments)
glimpse(host_countries)
glimpse(qualified_teams)


## 4. TIDY ----

### |- men's tournaments only ----
tournaments_clean <- tournaments |>
  mutate(is_womens = str_detect(tournament_name, "Women")) |>
  filter(!is_womens) |>
  select(tournament_id, year)

### |- ordinal performance tier, 0 (group) to 5 (champion) ----
performance_order <- c(
  "group stage"         = 0,
  "second group stage"  = 1.5,
  "round of 16"         = 1,
  "quarter-final"       = 2,
  "quarter-finals"      = 2,
  "final round"         = 2,
  "semi-finals"         = 3,
  "third-place match"   = 3,
  "final"               = 4
)

### |- host teams joined to their tournament finish ----
host_performance <- host_countries |>
  distinct(tournament_id, team_id, team_name) |>
  inner_join(tournaments_clean, by = "tournament_id") |>
  left_join(
    tournaments |> select(tournament_id, host_won),
    by = "tournament_id"
  ) |>
  left_join(
    qualified_teams |> select(tournament_id, team_id, performance),
    by = c("tournament_id", "team_id")
  ) |>
  mutate(
    perf_score = unname(performance_order[performance]),
    tier = case_when(
      host_won == 1 ~ 5, # champion
      perf_score == 4 ~ 4, # reached final, runner-up
      perf_score == 3 ~ 3, # semi-final / third-place match
      perf_score %in% c(2, 1.5) ~ 2, # quarter-final / final-round equiv.
      perf_score == 1 ~ 1, # round of 16
      TRUE ~ 0 # group stage
    )
  )

### |- one tile per TOURNAMENT ----
tile_data <- host_performance |>
  summarise(tier = max(tier), .by = c(tournament_id, year)) |>
  arrange(year) |>
  mutate(
    idx = row_number() - 1,
    ncol = 11,
    col = idx %% ncol,
    row = idx %/% ncol,
    plot_row = max(row) - row,
    color_bucket = case_when(
      tier %in% c(4, 5) ~ "final_or_champion",
      tier == 3 ~ "semi_final",
      tier %in% c(1, 2) ~ "r16_or_qf",
      TRUE ~ "group_stage"
    )
  )

# France 1998 callout anchor 
callout_tile <- tile_data |> filter(year == 1998)

# small chronological gap right after the France 1998 tile
# This makes "before/after 1998" readable without the viewer needing to
# understand that the strip wraps row-to-row.
gap_row <- callout_tile$row
gap_col <- callout_tile$col
gap_amount <- 0.4

tile_data <- tile_data |>
  mutate(x_pos = if_else(row == gap_row & col > gap_col, col + gap_amount, col))


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    final_or_champion = "#722F37",
    semi_final = "#A6717A",
    r16_or_qf = "#D8B9BD",
    group_stage = "#E4DFD6",
    accent = "#722F37", 
    neutral = "gray70"
  )
)

### |- titles and caption ----
title_text    <- "Hosting the World Cup Doesn't Take Teams as Far as It Once Did"

subtitle_text <- glue(
  "No World Cup host has reached the final since France in 1998. From ",
  "1930 through 2002, every host reached the knockout stage — and nearly ",
  "half reached the final."
)

caption_text <- create_swd_caption(
  year = 2026,
  month = "Jul",
  source_text = "Fjelstul World Cup Database (Joshua C. Fjelstul, Ph.D.)"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.title       = element_text(family = fonts$title_1, face = "bold", size = 20), margin = margin(b = 8),
    plot.subtitle    = element_textbox_simple(family = fonts$subtitle, size = 9, margin = margin(t = 4, b = 8)),
    plot.caption     = element_markdown(family = fonts$caption, size = 7, color = "grey40")
  )
)

theme_set(weekly_theme)

### |-  plot ----
legend_data <- tibble(
  bucket   = c("final_or_champion", "semi_final", "r16_or_qf", "group_stage"),
  label    = c("Finalist or champion", "Semifinal", "Quarterfinal / R16", "Group stage"),
  x_swatch = c(0, 4.4, 6.5, 9.1)
)

### |- panel 1: title + subtitle + real-tile legend ----
p_header <- ggplot() +
  geom_tile(
    data = legend_data, aes(x = x_swatch, y = 0, fill = bucket),
    width = 0.5, height = 0.5, color = NA
  ) +
  geom_text(
    data = legend_data, aes(x = x_swatch + 0.45, y = 0, label = label),
    hjust = 0, vjust = 0.5, size = 3, family = fonts$text, color = "grey30"
  ) +
  scale_fill_manual(values = clrs$palette, guide = "none") +
  coord_cartesian(xlim = c(-0.5, 14), ylim = c(-0.6, 0.6), clip = "off") +
  labs(title = title_text, subtitle = subtitle_text) +
  theme(
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), plot.margin = margin(t = 20, r = 40, b = 4, l = 40)
  )

### |- panel 2: the strip itself ----
p_main <- ggplot(tile_data, aes(x = x_pos, y = plot_row, fill = color_bucket)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.96, height = 0.96) +
  annotate("text",
    x = -0.8, y = max(tile_data$plot_row), label = "1930",
    hjust = 1, size = 4.3, family = fonts$text, color = "grey28"
  ) +
  annotate("text",
    x = 10.8 + gap_amount, y = 0, label = "2022",
    hjust = 0, size = 4.3, family = fonts$text, color = "grey28"
  ) +
  annotate("segment",
    x = callout_tile$col, xend = callout_tile$col,
    y = callout_tile$plot_row - 0.55, yend = callout_tile$plot_row - 1.0,
    linewidth = 0.3, color = "#722F37", alpha = 0.75
  ) +
  annotate("text",
    x = callout_tile$col, y = callout_tile$plot_row - 1.15,
    label = "Last host finalist\nFrance 1998",
    family = fonts$text, size = 2.3, lineheight = 0.95,
    hjust = 0.5, vjust = 1, color = "grey45"
  ) +
  scale_fill_manual(values = clrs$palette, guide = "none") +
  coord_equal(clip = "off") +
  theme(
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), plot.title = element_blank(), plot.subtitle = element_blank(),
    plot.margin = margin(t = 4, r = 40, b = 4, l = 40)
  )
  
### |- panel 3: caption only ----
caption_note <- "2002 tile reflects South Korea's semifinal — the deeper of the two co-host results (Japan reached the round of 16)."
caption_text_full <- glue("{caption_note}<br>{caption_text}")

p_caption <- ggplot() +
  labs(caption = caption_text_full) +
  theme_void() +
  theme(
    plot.caption = element_markdown(family = fonts$caption, size = 6, 
                                    color = "grey40", lineheight = 1.1),
    plot.margin  = margin(t = 4, r = 40, b = 10, l = 40)
  )

### |- compose ----
final_plot <- p_header / p_main / p_caption +
  plot_layout(heights = c(0.30, 1, 0.02))

### |-  preview ----
snap(final_plot)


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #SWDchallenge projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #SWDchallenge attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/SWDchallenge/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/tidytuesday/
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-07-01
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
#
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R.cache        0.17.0   2025-05-02 [1] CRAN (R 4.5.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.5.2)
# R.oo           1.27.1   2025-05-02 [1] CRAN (R 4.5.2)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rsconnect      1.8.0    2026-04-10 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────────
