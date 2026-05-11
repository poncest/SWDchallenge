
## Challenge: #SWDchallenge 2026-- May Exercise
## Topic:     move from dashboard to decision
## Author:    Steven Ponce
## Date:      2026-05-11


## 0. DATA SOURCE ----
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1ZX2MMENdwiwHvJsj34pr_6_FFBqz1KYc/edit?usp=sharing&ouid=101369070286981421257&rtpof=true&sd=true


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor,     
  scales, glue, patchwork      
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
raw_data <- tibble(
  month    = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"),
  val_2024 = c(36, 34, 55, 39, 43, 31, 38, 37, 55),
  val_2025 = c(63, 61, 38, 46, 37, 52, 60, 37, 41)
)


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----
df <- raw_data |>
  mutate(
    delta = val_2025 - val_2024,
    arrow_color = if_else(
      month %in% c("JAN", "FEB"),
      "hero",    # dark teal
      "other"    # mid teal
    ),
    hjust_2025 = if_else(delta >= 0, -0.45, 1.5),
    month = fct_inorder(month)
  )

# Verified KPI values
total_delta   <- sum(df$delta)       # +67K net (Jan–Sep)

jan_feb_delta <- df |>
  filter(month %in% c("JAN","FEB")) |>
  pull(delta) |> sum()               # +54K

# Selective 2025 endpoint labels: hero + JUL + notable declines
df_labeled <- df |> filter(month %in% c("JAN","FEB","JUL","MAR","SEP"))



## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    hero      = "#1A6B72",  
    other     = "#5BAAB0",  
    ref_dot   = "#9DADB5",   
    text_main = "#1C2B33",
    text_sub  = "#6B7B85",
    text_hero = "#1A6B72",
    bg        = "#FAFAFA"
  )
)

col_hero      <- colors$palette$hero
col_other     <- colors$palette$other
col_ref_dot   <- colors$palette$ref_dot
col_text_main <- colors$palette$text_main
col_text_sub  <- colors$palette$text_sub
col_text_hero <- colors$palette$text_hero
col_bg        <- colors$palette$bg

arrow_colors  <- c(hero = col_hero, other = col_other)

### |-  titles and caption ----
title_text <- "Jan and Feb Drove Most of 2025's Refer-A-Friend Growth"

subtitle_text <- glue(
  "Jan and Feb accounted for over 80% of the 2025 year-to-date increase versus 2024. ",
  "Arrows show change from 2024 (gray dot) to 2025.<br>",
  "<span style='color:{col_text_sub}'>",
  "*2025 data shown through September only \u2014 Oct\u2013Dec not yet available.*",
  "</span>"
)

caption_text <- create_swd_exe_caption(
  year        = 2026,
  month       = "May",
  source_text = "Refer-A-Friend customer service dashboard · Exercise 6.11, Let's Practice! (Cole Nussbaumer Knaflic)"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(
      size = 9.5, color = col_text_sub, family = fonts$text, face = "bold"
    ),
    axis.text.x = element_text(
      size = 8, color = col_text_sub, family = fonts$text
    ),
    axis.title = element_blank(),
    plot.margin = margin(8, 8, 12, 12)
  )
)

theme_set(weekly_theme)


### |-  arrow chart ----

df_segments <- df |> filter(delta != 0)
df_flat     <- df |> filter(delta == 0)

p_arrow <- ggplot() +
  geom_point(
    data  = df,
    aes(x = val_2024, y = month),
    color = col_ref_dot,
    size  = 2.2,
    shape = 16
  ) +
  geom_segment(
    data = df_segments,
    aes(
      x     = val_2024,
      xend  = val_2025,
      y     = month,
      yend  = month,
      color = arrow_color
    ),
    arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
    linewidth = 0.95,
    lineend = "butt"
  ) +
  geom_point(
    data  = df_flat,
    aes(x = val_2025, y = month),
    color = col_other,
    size  = 2.5,
    shape = 16
  ) +
  geom_text(
    data = df |> filter(delta >= 0),
    aes(x = val_2024, y = month, label = glue("{val_2024}K")),
    color = col_text_sub,
    hjust = 1.65,
    size = 2.3,
    family = fonts$text
  ) +
  geom_text(
    data = df |> filter(delta < 0),
    aes(x = val_2024, y = month, label = glue("{val_2024}K")),
    color = col_text_sub,
    hjust = -0.45,
    size = 2.3,
    family = fonts$text
  ) +
  geom_text(
    data = df |> filter(delta > 0),
    aes(
      x     = val_2025,
      y     = month,
      label = glue("{val_2025}K"),
      color = arrow_color
    ),
    hjust = -0.45,
    size = 2.7,
    fontface = "bold",
    family = fonts$text
  ) +
  geom_text(
    data = df |> filter(delta < 0),
    aes(
      x     = val_2025,
      y     = month,
      label = glue("{val_2025}K"),
      color = arrow_color
    ),
    hjust = 1.0,
    nudge_x = -1.8,
    size = 2.7,
    fontface = "bold",
    family = fonts$text
  ) +
  geom_text(
    data = df_flat,
    aes(x = val_2025, y = month, label = glue("{val_2025}K")),
    color = col_other,
    hjust = -0.55,
    size = 2.4,
    family = fonts$text
  ) +
  geom_label(
    data = tibble(
      x = 73, y = 8.5,
      label = "Jan + Feb\nadded 54K of the\n67K YTD increase\nvs. 2024"
    ),
    aes(x = x, y = y, label = label),
    color = col_text_main,
    fill = "#E8F4F5",
    label.color = col_hero,
    label.size = 0.5,
    label.padding = unit(0.55, "lines"),
    size = 2.4,
    hjust = 0.5,
    lineheight = 1.4,
    family = fonts$text,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = 19, y = 9.45,
    label = "Refer-A-Friend account volume (thousands)  \u2022  2024 vs 2025",
    color = col_text_sub, size = 2.3, hjust = 0,
    family = fonts$text
  ) +
  scale_color_manual(values = arrow_colors, guide = "none") +
  scale_x_continuous(
    limits = c(18, 82),
    breaks = c(20, 40, 60, 80),
    expand = c(0, 0),
    labels = function(x) paste0(x, "K")
  ) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(clip = "off")


### |-  right editorial panel ----

right_df <- tibble(x = 0.5, y = 0.5)

p_right <- ggplot(right_df, aes(x, y)) +

  # YEAR-TO-DATE CONTEXT
  geom_text(aes(x = 0.5, y = 0.95),
    label = "YEAR-TO-DATE CONTEXT", color = col_text_hero,
    size = 2.3, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.87),
    label = "2025 YTD is running\n18% above 2024",
    color = col_text_main, size = 2.7, hjust = 0.5,
    lineheight = 1.35, family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.25, y = 0.78),
    label = "2024\nJan\u2013Sep", color = col_text_sub,
    size = 2.2, hjust = 0.5, lineheight = 1.2,
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.75, y = 0.78),
    label = "2025\nJan\u2013Sep", color = col_text_sub,
    size = 2.2, hjust = 0.5, lineheight = 1.2,
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.25, y = 0.68),
    label = "368K", color = col_text_main,
    size = 4.8, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.75, y = 0.68),
    label = "435K", color = col_hero,
    size = 4.8, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.59),
    label = "Oct\u2013Dec not yet available",
    color = col_text_sub, size = 1.9, hjust = 0.5,
    fontface = "italic", family = fonts$text, inherit.aes = FALSE
  ) +
  annotate("segment",
    x = 0.05, xend = 0.95, y = 0.53, yend = 0.53,
    color = "gray83", linewidth = 0.4
  ) +

  # WHAT TO INVESTIGATE ────
  geom_text(aes(x = 0.5, y = 0.47),
    label = "WHAT TO INVESTIGATE", color = col_text_hero,
    size = 2.3, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.31),
    label = "Did campaign timing, referral\nincentives, or operational\nchanges drive the Q1 spike?\nIs the effect temporary\nor structural?",
    color = col_text_main, size = 2.7, hjust = 0.5,
    lineheight = 1.45, family = fonts$text, inherit.aes = FALSE
  ) +
  annotate("segment",
    x = 0.05, xend = 0.95, y = 0.12, yend = 0.12,
    color = "gray83", linewidth = 0.4
  ) +
  geom_text(aes(x = 0.5, y = 0.06),
    label = "Jun and Jul also elevated\nbut within historical range.",
    color = col_text_sub, size = 2.0, hjust = 0.5,
    lineheight = 1.3, family = fonts$text, inherit.aes = FALSE
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_bw() +
  theme(
    plot.background  = element_rect(fill = "#EEF1F3", color = "gray83", linewidth = 0.5),
    panel.background = element_rect(fill = "#EEF1F3", color = NA),
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    plot.margin      = margin(14, 16, 14, 16)
  )


### |-  combined plots ----
p_slide <- p_arrow + p_right +
  plot_layout(widths = c(0.68, 0.32)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = 14, face = "bold", color = col_text_main,
        family = fonts$title, margin = margin(b = 4)
      ),
      plot.subtitle = element_markdown(
        size = 9, color = col_text_main, family = fonts$text,
        lineheight = 1.4, margin = margin(b = 14)
      ),
      plot.caption = element_markdown(
        size = 6, color = "#9DADB5", family = fonts$text,
        hjust = 0, margin = margin(t = 8)
      ),
      plot.background = element_rect(fill = col_bg, color = NA),
      plot.margin = margin(16, 16, 10, 16)
    )
  )

### |-  Preview ----
snap(p_slide)

  
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


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-11
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
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
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────────