
## Challenge: #SWDchallenge 2026-- May (Exercise 6.3)
## Topic:     move from dashboard to decision
## Author:    Steven Ponce
## Date:      2026-05-14


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
raw_data <- readxl::read_excel(
  "2026/Ex_063/EX063 - from dashboard to decision.xlsx",
  sheet = "DATA", range = "B4:Q16", trim_ws = TRUE)


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----

df <- raw_data |>
  clean_names() |>
  mutate(
    month      = fct_inorder(month),
    month_num  = row_number(),

    # Hero metric: SLA vs goal
    sla        = overall_result,
    goal_sla   = goal,
    sla_miss   = sla < goal_sla,

    # Supporting: answer speed vs 8s goal
    speed      = avg_answer_speed_seconds,
    goal_speed = 8.0,
    speed_over = speed - goal_speed,

    # WIP: sum of all inventory components
    wip_total  = requests + orders + pending_e_file + unprocessed_inbox,
    wip_target = target_inventory
  )

# Annotation data
anno_pts <- tibble(
  month = factor(c("APR", "NOV", "DEC"), levels = levels(df$month)),
  month_num = c(4, 11, 12),
  sla = c(0.82, 0.83, 0.97)
)


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ---
colors <- get_theme_colors(
  palette = list(
    sla_line  = "#2E4057",   
    goal_line = "#722F37",  
    miss_fill = "#F5EAEB",   
    anno_fail = "#722F37",   
    anno_dec  = "#2E4057",   
    text_main = "#1C2B33",   
    text_sub  = "#6B7B85",   
    text_hero = "#2E4057",   
    bg        = "#FAFAFA"   
  )
)

col_sla_line <- colors$palette$sla_line
col_goal_line <- colors$palette$goal_line
col_miss_fill <- colors$palette$miss_fill
col_anno_nov <- colors$palette$anno_fail
col_anno_dec <- colors$palette$anno_dec
col_text_main <- colors$palette$text_main
col_text_sub <- colors$palette$text_sub
col_text_hero <- colors$palette$text_hero
col_bg <- colors$palette$bg

### |-  titles and caption ----
title_text <- "April and November Expose the Clearest Capacity Strain"

subtitle_text <- glue(
  "Service levels missed the 90% target in 6 of 12 months. ",
  "April and November were the clearest operational failure points.<br>",
  "By December, customer-facing performance recovered \u2014 but backlog surged behind the scenes."
)

caption_text <- create_swd_caption(
  year = 2026,
  month  = "May",
  source_text = "Customer service dashboard · Exercise 6.3, Let's Practice! (Cole Nussbaumer Knaflic)"
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
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      size = 9, color = col_text_sub, family = fonts$text, face = "bold"
    ),
    axis.text.y = element_text(
      size = 8, color = col_text_sub, family = fonts$text
    ),
    axis.title = element_blank(),
    plot.margin = margin(8, 12, 8, 12)
  )
)

theme_set(weekly_theme)

### |-  hero chart: SLA vs goal ----

p_sla <- df |>
  ggplot(aes(x = month_num, y = sla)) +

  # Geoms
  geom_ribbon(
    aes(ymin = pmin(sla, goal_sla), ymax = goal_sla),
    fill = col_miss_fill,
    alpha = 0.6
  ) +
  geom_hline(
    yintercept = 0.90,
    color      = col_goal_line,
    linewidth  = 0.6,
    linetype   = "dashed"
  ) +
  geom_line(
    color     = col_sla_line,
    linewidth = 1.1
  ) +
  geom_point(
    color = col_sla_line,
    size  = 2.5,
    shape = 16
  ) +
  geom_point(
    data = anno_pts |> filter(month == "APR"),
    aes(x = month_num, y = sla),
    color = col_anno_nov, size = 4.5, shape = 16
  ) +
  geom_point(
    data = anno_pts |> filter(month == "NOV"),
    aes(x = month_num, y = sla),
    color = col_anno_nov, size = 4.5, shape = 16
  ) +
  geom_point(
    data = anno_pts |> filter(month == "DEC"),
    aes(x = month_num, y = sla),
    color = col_anno_dec, size = 4.5, shape = 16
  ) +
  # Annotate
  annotate("text",
    x = 4, y = 0.82,
    label = "9.4s answer speed",
    color = col_anno_nov, size = 2.1, vjust = 4.0, hjust = 0.5,
    family = fonts$text
  ) +
  annotate("text",
    x = 4, y = 0.82,
    label = "APR 82%",
    color = col_anno_nov, size = 2.7, vjust = 2.2, hjust = 0.5,
    fontface = "bold", family = fonts$text
  ) +
  annotate("text",
    x = 11, y = 0.83,
    label = "10.1s \u2014 worst of year",
    color = col_anno_nov, size = 2.1, vjust = 4.0, hjust = 0.5,
    family = fonts$text
  ) +
  annotate("text",
    x = 11, y = 0.83,
    label = "NOV 83%",
    color = col_anno_nov, size = 2.7, vjust = 2.2, hjust = 0.5,
    fontface = "bold", family = fonts$text
  ) +
  annotate("text",
    x = 12, y = 0.97,
    label = "DEC 97%",
    color = col_anno_dec, size = 2.7, vjust = -0.6, hjust = 1.2,
    fontface = "bold", family = fonts$text
  ) +
  annotate("text",
    x = 12, y = 0.972,
    label = "Backlog: 2,147 units",
    color = col_anno_dec, size = 1.9, vjust = -2.2, hjust = 1.2,
    family = fonts$text
  ) +
  annotate("text",
    x = 0.5, y = 0.905,
    label = "90% goal", color = col_goal_line,
    size = 2.3, hjust = 0, vjust = 0,
    family = fonts$text
  ) +
  # Scales
  scale_x_continuous(
    breaks = 1:12,
    labels = c(
      "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
    ),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    limits = c(0.76, 1.00),
    breaks = c(0.80, 0.85, 0.90, 0.95, 1.00),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off")


### |-  right diagnostic panel ----

right_df <- tibble(x = 0.5, y = 0.5)

p_right <- ggplot(right_df, aes(x, y)) +

  # Geoms
  geom_text(aes(x = 0.5, y = 0.95),
    label = "CHRONIC BASELINE", color = col_text_hero,
    size = 2.3, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.87),
    label = "Answer speed exceeded\nthe 8-second goal\nin all 12 months",
    color = col_text_main, size = 2.7, hjust = 0.5,
    lineheight = 1.35, family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.73),
    label = "Worst: Nov at 10.1s\n(+2.1s above goal)",
    color = col_anno_nov, size = 3.0, hjust = 0.5,
    fontface = "bold", lineheight = 1.3,
    family = fonts$text, inherit.aes = FALSE
  ) +
  annotate("segment",
    x = 0.05, xend = 0.95, y = 0.63, yend = 0.63,
    color = "gray83", linewidth = 0.4
  ) +

  # DOWNSTREAM CONSEQUENCE
  geom_text(aes(x = 0.5, y = 0.57),
    label = "DOWNSTREAM CONSEQUENCE", color = col_text_hero,
    size = 2.3, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.44),
    label = "Dec SLA recovered to 97%,\nbut WIP backlog surged\nto 2,147 units \u2014\n26% above the 1,700 target.\nJun and Jul also drifted below\ntarget, signaling earlier strain.",
    color = col_text_main, size = 2.7, hjust = 0.5,
    lineheight = 1.4, family = fonts$text, inherit.aes = FALSE
  ) +
  annotate("segment",
    x = 0.05, xend = 0.95, y = 0.24, yend = 0.24,
    color = "gray83", linewidth = 0.4
  ) +

  #  WHAT TO INVESTIGATE
  geom_text(aes(x = 0.5, y = 0.18),
    label = "WHAT TO INVESTIGATE", color = col_text_hero,
    size = 2.3, hjust = 0.5, fontface = "bold",
    family = fonts$text, inherit.aes = FALSE
  ) +
  geom_text(aes(x = 0.5, y = 0.07),
    label = "Are staffing levels and\nworkflow capacity planned\naround Apr and Nov\npeak demand periods?",
    color = col_text_main, size = 2.7, hjust = 0.5,
    lineheight = 1.4, family = fonts$text, inherit.aes = FALSE
  ) +

  # Scales
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +

  # Theme
  theme_bw() +
  theme(
    plot.background  = element_rect(fill = "#EEF1F3", color = "gray83", linewidth = 0.5),
    panel.background = element_rect(fill = "#EEF1F3", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(14, 16, 14, 16)
  )


### |-  combined plots ----
p_slide <- p_sla + p_right +
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

# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-14
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
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
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.5.3)
# rematch        2.0.0    2023-08-30 [1] CRAN (R 4.5.3)
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
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────