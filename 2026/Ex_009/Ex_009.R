
## Challenge: #SWDchallenge 2026 - Course Exercise -- Choosing an Appropriate Graph
## Topic:     explore visual possibilities
## Author:    Steven Ponce
## Date:      2026-09-08


## 0. DATA SOURCE ----
#' Data: Customer service key metrics, full year (Jan–Dec)
## Exercise 6.3, Let's Practice! — storytellingwithdata.com
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1zEes0KGHEP1sJfIHgAT2FFzpFMCp0i6e/edit?usp=drive_link&ouid=101369070286981421257&rtpof=true&sd=true
#' 

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,     
  scales, glue, ggview   
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
purchases_raw <- tibble::tribble(
  ~promotion, ~type,     ~count,
  "A",        "New",       315,
  "A",        "Existing", 1458,
  "B",        "New",       415,
  "B",        "Existing", 1265,
  "C",        "New",       263,
  "C",        "Existing", 1368
)


## 3. EXAMINE THE DATA ----
glimpse(purchases_raw)


## 4. TIDY ----

share_data <- purchases_raw |>
  pivot_wider(names_from = type, values_from = count) |>
  mutate(total = New + Existing, new_share = New / total)

overall_share <- sum(share_data$New) / sum(share_data$total) # 0.1953

plot_data <- share_data |>
  mutate(
    diff_pp   = (new_share - overall_share) * 100, # in percentage points
    promotion = fct_reorder(promotion, diff_pp),
    above     = diff_pp > 0
  )


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ---
clrs <- list(accent = "#722F37", muted = "gray60")
point_values <- c(`TRUE` = clrs$accent, `FALSE` = clrs$muted)

### |-  titles and caption ----
title_text <- glue(
  "<span style='color:{clrs$accent}'><b>Promotion B</b></span> had the ",
  "highest share of new customers"
)

subtitle_text <- glue(
  "New customers made up 24.7% of B's purchasers \u2014 5.2 percentage points above the overall mix; A and C were below it."
)

caption_text <- glue(
  "Customer mix among purchasers; promotion reach was not provided, ",
  "so these are not conversion rates.<br>",
  create_swd_caption(
    year = 2026, month = "Sep",
    source_text = "SWD course exercise: purchase activity by promotion & customer type"
  )
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

base_theme <- create_base_theme(get_theme_colors(palette = list()))
dev_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 11, family = fonts$text),
    axis.text = element_text(size = 10, family = fonts$text),
    plot.title = element_textbox_simple(
      size = 20,
      face = "bold",
      family = fonts$title_1,
      color = "#2C2825",
      lineheight = 1.1,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      size = 12.5,
      family = fonts$subtitle,
      color = "#7A7068",
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      size = 6.0,
      family = fonts$caption,
      color = "#7A7068",
      margin = margin(t = 12)
    )
  )
)

theme_set(dev_theme)

### |-  plot ----
p <- ggplot(plot_data, aes(x = diff_pp, y = promotion)) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = diff_pp, yend = promotion, color = above),
    linewidth = 1.1
  ) +
  geom_point(aes(color = above), size = 5) +
  geom_text(
    aes(label = paste0(label_number(style_positive = "plus", accuracy = 0.1)(diff_pp), " pp")),
    hjust = "outward", nudge_x = ifelse(plot_data$diff_pp >= 0, 0.35, -0.35),
    size = 3.6, family = fonts$text, color = "gray20"
  ) +
  annotate(
    "text",
    x = 0, y = 3.4,
    label = "Overall purchaser mix\n19.5% new",
    hjust = -0.05, size = 3.1, color = "gray30", family = fonts$text,
    lineheight = 0.95
  ) +
  scale_color_manual(values = point_values, guide = "none") +
  scale_x_continuous(
    breaks = seq(-5, 5, 2.5),
    labels = function(x) {
      val <- abs(x)
      val_str <- ifelse(val == floor(val), sprintf("%.0f", val), sprintf("%.1f", val))
      sign <- ifelse(x > 0, "+", ifelse(x < 0, "\u2212", ""))
      paste0(sign, val_str)
    },
    expand = expansion(mult = 0.15)
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Difference from overall new-customer share", y = NULL
  )


### |- preview ----
p +
  canvas(width = 11, height = 6.5, units = "in", dpi = 300)


## 6. SAVE ----
save_ggplot(
  plot = p,
  file = "2026/Ex_009/img/Ex_009.png",
  width = 11, height = 6.5
)

# 7. HELPER FUNCTIONS DOCUMENTATION ----

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
#    • create_swd_caption()     - Generate formatted caption with social handles
#    . create_swd_exe_caption()   and #SWDchallenge attribution
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
#   1. Clone the repo: https://github.com/poncest/SWDchallenge
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


# 8. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-08
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.6.0)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────
# > 
