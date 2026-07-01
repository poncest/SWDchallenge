
## Challenge: #SWDchallenge 2026
## Topic:     JUN 2026 | when normal is noteworthy

##
## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   Carrier On-Time Performance Dataset
##   https://www.kaggle.com/datasets/mexwell/carrier-on-time-performance-dataset


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, scales, glue
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 11,
  height = 6.5,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- read_csv(
  here::here("2026/06_Jun/airline_2m.csv"),
  show_col_types = FALSE
) |> clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----

monthly <- raw_data |>
  filter(year >= 2013, year <= 2019) |>
  transmute(year, month, operated = cancelled == 0, on_time = arr_del15 == 0) |>
  summarise(ontime_rate = mean(on_time[operated], na.rm = TRUE), .by = c(year, month)) |>
  arrange(year, month)

band_all <- monthly |> summarise(lo = min(ontime_rate), hi = max(ontime_rate), .by = month)
avg_path <- monthly |> summarise(ontime_rate = mean(ontime_rate), .by = month)


### |-  anchor coordinates ----
jun_y <- avg_path$ontime_rate[avg_path$month == 6]


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    band = "#722F37", 
    edge = "#B89AA0", 
    line = "#722F37",
    anno = "gray35", 
    sub = "gray35"
  )
)

col_band <- clrs$palette$band
col_edge <- clrs$palette$edge
col_line <- clrs$palette$line
col_anno <- clrs$palette$anno
col_sub  <- clrs$palette$sub

### |- titles and caption ----
title_text <- "Summer delays arrive right on schedule"

subtitle_text <- glue(
  "Air travel feels chaotic \u2014 weather, holiday crushes, the summer storm season.<br>",
  "Yet on-time arrivals follow nearly the same seasonal pattern every year. ",
  "Summer's dip is predictable \u2014 and not a reason to change course."
)

caption_text <- create_swd_caption(
  year = 2026, month = "Jun",
  source_text = "U.S. DOT, Bureau of Transportation Statistics \u2014 On-Time Performance (2M-flight sample, 2013\u201319)"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0)
  )
)

theme_set(weekly_theme)

### |-  plot ----
p <- ggplot() +
  # Geoms
  geom_ribbon(
    data = band_all, aes(month, ymin = lo, ymax = hi),
    fill = col_band, alpha = 0.12
  ) +
  geom_line(data = band_all, aes(month, lo), color = col_edge, linewidth = 0.4) +
  geom_line(data = band_all, aes(month, hi), color = col_edge, linewidth = 0.4) +
  geom_line(data = avg_path, aes(month, ontime_rate), color = col_line, linewidth = 1.1) +
  geom_point(data = avg_path, aes(month, ontime_rate), color = col_line, size = 2.4) +
  # Annotate
  annotate("text",
    x = 6, y = jun_y, vjust = 2.2, hjust = 0.5,
    family = fonts$text, size = 3.4, lineheight = 0.95, color = col_anno,
    label = "~76% on-time, every summer\nseven years running"
  ) +
  annotate("text",
    x = 1, y = band_all$hi[band_all$month == 1], hjust = 0, vjust = -0.7,
    family = fonts$text, size = 3, fontface = "italic", color = col_anno,
    label = "Historical range, 2013\u201319"
  ) +
  # Scales
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  # Labs
  labs(
    title = title_text, subtitle = subtitle_text,
    x = NULL, y = "On-time arrivals", caption = caption_text
  ) +
  coord_cartesian(clip = "off") +
  # Theme
  theme(
    plot.title = element_markdown(
      size = 26, face = "bold", family = fonts$title_1,
      color = clrs$palette$title, margin = margin(b = 10), lineheight = 1.2
    ),
    plot.subtitle = element_textbox_simple(
      size = 11, family = fonts$text, color = col_sub,
      lineheight = 1.5, margin = margin(b = 20)
    ),
    plot.caption = element_markdown(
      size = 6.5, family = fonts$caption, color = col_sub,
      hjust = 0, margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = clrs$palette$bg, color = NA),
    plot.margin = margin(16, 16, 12, 16)
  )

### |-  preview ----
snap(p)


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
# 
# ─ Session info ──────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-01
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr          1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2        4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here           1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr          1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R.cache        0.17.0   2025-05-02 [1] CRAN (R 4.5.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.5.2)
# R.oo           1.27.1   2025-05-02 [1] CRAN (R 4.5.2)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales         1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble         3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────