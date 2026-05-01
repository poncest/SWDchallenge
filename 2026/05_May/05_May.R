
## Challenge: #SWDchallenge 2026 -- May
## Topic:     MAY 2026 | human + AI

##
## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   Our World in Data — Energy Mix
##   Hannah Ritchie, Pablo Rosado and Max Roser (2024)
##   https://ourworldindata.org/energy
##   Cache: 2026/05_May/owid-energy-data.csv


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,     
  scales, glue      
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

### |- observed CO₂: read from local cache ----
raw_data <- read_csv(
  here::here("2026/05_May/owid-energy-data.csv"),
  show_col_types = FALSE
)


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----

### |- define year window ----
year_start <- 2010
year_end <- 2023

### |- countries to include ----
# G20 + notable peers; filtered to those with complete data in both endpoints
# Excludes aggregates (EU, World) and small island states
selected_countries <- c(
  "Australia", "Brazil", "Canada", "Chile", "China",
  "Colombia", "Denmark", "France", "Germany", "India",
  "Indonesia", "Italy", "Japan", "Mexico", "Netherlands",
  "Poland", "Portugal", "South Africa", "South Korea",
  "Spain", "Sweden", "Türkiye", "United Kingdom",
  "United States", "Vietnam"
)

### |- extract endpoint values ----
df_endpoints <- raw_data |>
  filter(
    country %in% selected_countries,
    year %in% c(year_start, year_end),
    !is.na(fossil_share_elec)
  ) |>
  select(country, year, fossil_share_elec) |>
  pivot_wider(
    names_from = year,
    values_from = fossil_share_elec,
    names_prefix = "fossil_"
  ) |>
  # drop any country missing either endpoint
  filter(!is.na(fossil_2010), !is.na(fossil_2023))

### |- compute annualized exit rate ----
n_years <- year_end - year_start

df_rates <- df_endpoints |>
  mutate(
    # total pp change (negative = exit; positive = backslide)
    pp_change = fossil_2023 - fossil_2010,
    # annualized rate in pp/year
    annual_rate = pp_change / n_years,
    # tier based on annual decline rate — 3 tiers for visual clarity
    tier = case_when(
      annual_rate <= -1.5 ~ "Fast",
      annual_rate < 0 ~ "Slow",
      TRUE ~ "Stalled / Backsliding"
    ),
    tier = factor(tier, levels = c(
      "Fast", "Slow", "Stalled / Backsliding"
    ))
  ) |>
  arrange(annual_rate) |>
  mutate(country = fct_inorder(country))

### |- label data with outward positioning ----
df_label <- df_rates |>
  filter(
    row_number() <= 3 |
      row_number() > (n() - 3) |
      annual_rate > 0
  ) |>
  distinct(country, .keep_all = TRUE) |>
  mutate(
    label_x = case_when(
      annual_rate < 0 ~ annual_rate - 0.10,
      annual_rate > 0 ~ annual_rate + 0.10,
      TRUE ~ annual_rate
    ),
    label_hjust = case_when(
      annual_rate < 0 ~ 1,
      annual_rate > 0 ~ 0,
      TRUE ~ 0.5
    )
  )

### |- tier separator y-positions ----
df_tier_breaks <- df_rates |>
  mutate(y_int = as.integer(country)) |>
  group_by(tier) |>
  summarise(y_min = min(y_int), .groups = "drop") |>
  filter(tier != first(levels(df_rates$tier))) |>
  mutate(separator = y_min - 0.5)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "background"  = "#F5F3EE",
    "text"        = "#2C2C2C",
    "subtext"     = "#5C5C5C",
    "grid"        = "#E8E5DF",
    "fast"        = "#1A6B3C",
    "slow"        = "#A8C5A0",
    "stalled"     = "#8B3030"
  )
)

bg_col <- colors$palette$background
text_col <- colors$palette$text
sub_col <- colors$palette$subtext
grid_col <- colors$palette$grid

# tier color lookup — 3 tiers
tier_colors <- c(
  "Fast" = colors$palette$fast,
  "Slow" = colors$palette$slow,
  "Stalled / Backsliding" = colors$palette$stalled
)

### |- titles and caption ----
title_text <- "Some Countries Are Exiting Fossil Fuels Fast. Others Are Standing Still."

subtitle_text <- glue(
  "Annualized change in fossil fuel share of electricity generation, ",
  "{year_start}–{year_end} (percentage points per year).<br>",
  "Leftward arrows signal progress; rightward arrows signal backsliding."
)

caption_text <- create_swd_caption(
  year = 2026,
  month = "May",
  source_text = "Our World in Data | Energy Mix (Ritchie, Rosado & Roser, 2024)"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),

    # grid: vertical only
    panel.grid.major.x = element_line(color = grid_col, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # axes
    axis.title.x = element_text(
      color = sub_col, size = 8.5, family = fonts$text,
      margin = margin(t = 6)
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = sub_col, size = 8, family = fonts$text),
    axis.text.y = element_text(
      color = text_col, size = 8.5, family = fonts$text, hjust = 1
    ),
    axis.ticks = element_blank(),

    # no legend 
    legend.position = "none",

    # titles
    plot.title = element_text(
      family = fonts$title, face = "bold",
      size = 17, color = text_col,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      family = fonts$text, size = 9, color = sub_col,
      lineheight = 1.45, margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      family = fonts$text, size = 7.5, color = sub_col,
      hjust = 0, margin = margin(t = 12)
    ),
    plot.margin = margin(20, 24, 14, 60)
  )
)

theme_set(weekly_theme)

### |- tier bracket annotations ----
df_tier_labels <- df_rates |>
  mutate(y_int = as.integer(country)) |>
  group_by(tier) |>
  summarise(
    y_top = max(y_int) + 0.35,
    y_bot = min(y_int) - 0.35,
    y_mid = (max(y_int) + min(y_int)) / 2,
    .groups = "drop"
  ) |>
  mutate(
    label_color = tier_colors[as.character(tier)],
    label_text = case_when(
      tier == "Stalled / Backsliding" ~ "Stalled /\nBacksliding",
      TRUE ~ as.character(tier)
    ),
    y_label = case_when(
      tier == "Stalled / Backsliding" ~ y_mid - 0.10,
      tier == "Fast" ~ y_mid + 0.10,
      TRUE ~ y_mid
    ),
    label_color = case_when(
      tier == "Slow" ~ "#7A9478",
      TRUE ~ label_color
    )
  )

### |- main plot ----
p <- ggplot(df_rates, aes(y = country, color = tier)) +

  # Geoms
  geom_hline(
    data = df_tier_breaks,
    aes(yintercept = separator),
    color = grid_col,
    linewidth = 0.45,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  geom_vline(
    xintercept = 0,
    color = sub_col,
    linewidth = 0.8,
    linetype = "solid"
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = annual_rate,
      yend = country,
      alpha = tier
    ),
    linewidth = 0.7,
    lineend = "butt",
    arrow = arrow(
      length = unit(0.18, "cm"),
      type = "closed"
    )
  ) +
  geom_text(
    data = df_label,
    aes(
      x = label_x,
      y = country,
      label = sprintf("%+.1f", annual_rate),
      hjust = label_hjust
    ),
    vjust = 0.5,
    size = 2.8,
    family = fonts$text,
    color = text_col,
    fontface = "plain",
    inherit.aes = FALSE
  ) +
  # Annotate
  annotate(
    "segment",
    x = -4.45,
    xend = -4.45,
    y = df_tier_labels$y_bot,
    yend = df_tier_labels$y_top,
    color = df_tier_labels$label_color,
    linewidth = 0.4,
    alpha = 0.75
  ) +
  annotate(
    "text",
    x = -4.36,
    y = df_tier_labels$y_label,
    label = df_tier_labels$label_text,
    color = df_tier_labels$label_color,
    size = 2.25,
    hjust = 0,
    vjust = 0.5,
    fontface = "bold",
    family = fonts$text
  ) +

  # Scales
  scale_color_manual(values = tier_colors) +
  scale_alpha_manual(
    values = c(
      "Fast" = 1,
      "Slow" = 0.65,
      "Stalled / Backsliding" = 1
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    name = "Change in fossil fuel share (pp per year)",
    labels = label_number(suffix = " pp", style_positive = "plus"),
    breaks = c(-4, -3, -2, -1, 0, 1),
    limits = c(-4.75, 1.20),
    expand = expansion(mult = c(0, 0))
  ) +
  guides(color = "none") +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = NULL
  )

### |- preview ----
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

# ─ Session info ─────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-01
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
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
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
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
# ────────────────────────────────────────────────────────────────────