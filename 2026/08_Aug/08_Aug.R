
## Challenge: #SWDchallenge 2026 
## Topic:     AUG 2026 | whip up a waterfall
## Author:    Steven Ponce
## Date:      2026-08-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   Carrier On-Time Performance Dataset
##   https://www.kaggle.com/datasets/mexwell/carrier-on-time-performance-dataset


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, scales, glue, patchwork, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

## No external file. Figures are sourced directly from the published report:
## BIO, Informa Pharma Intelligence & QLS Advisors. "Clinical Development
## Success Rates and Contributing Factors 2011-2020." February 2021.
## 12,728 phase transitions across 9,704 development programs, 2011-2020.
##
## Phase transition success rates ("all indications"):
##   Phase I   -> Phase II   : 52.0% (n = 4,414)
##   Phase II  -> Phase III  : 28.9% (n = 4,933)   
##   Phase III -> NDA/BLA    : 57.8% (n = 1,928)
##   NDA/BLA   -> Approval   : 90.6% (n = 1,453)  
##
## Overall likelihood of approval (LOA) from Phase I: 7.9% (n = 12,728)

phase_success_rates <- tribble(
  ~stage,        ~success_rate,
  "phase_1",     0.520,
  "phase_2",     0.289,
  "phase_3",     0.578,
  "nda_bla",     0.906
)


## 3. EXAMINE THE DATA ----
glimpse(phase_success_rates)


## 4. TIDY ----

## Apply the published rates to a hypothetical starting cohort of 1,000
## Phase I programs. This is a compounded-probability illustration, NOT a
## single tracked group of real drugs -- the "n" values above are
## transition-specific denominators from the full 2011-2020 dataset, not
## one cohort's headcount at each stage. Disclosed in the caption.

cohort_start <- 1000

cohort_totals <- phase_success_rates |>
  mutate(
    running_total = cohort_start * cumprod(success_rate),
    running_total = round(running_total)
  ) |>
  pull(running_total)

## Build the waterfall geometry
waterfall_data <- tibble(
  stage = c("Start", "Phase I", "Phase II", "Phase III", "NDA/BLA", "Approved"),
  bar_type = c("total", "loss", "loss", "loss", "loss", "total"),
  ymin = c(0, cohort_totals[1], cohort_totals[2], cohort_totals[3], cohort_totals[4], 0),
  ymax = c(1000, 1000, cohort_totals[1], cohort_totals[2], cohort_totals[3], cohort_totals[4])
) |>
  mutate(
    stage = fct_inorder(stage),
    x_pos = row_number(),
    bar_label = c(
      "1,000",
      glue("\u2212{comma(1000 - cohort_totals[1])}"),
      glue("\u2212{comma(cohort_totals[1] - cohort_totals[2])}"),
      glue("\u2212{comma(cohort_totals[2] - cohort_totals[3])}"),
      glue("\u2212{comma(cohort_totals[3] - cohort_totals[4])}"),
      comma(cohort_totals[4])
    ),
    is_accent = stage == "Phase II"
  )

## Connector segments
connector_data <- tibble(
  x    = waterfall_data$x_pos[1:5] + 0.4,
  xend = waterfall_data$x_pos[2:6] - 0.4,
  y    = c(1000, cohort_totals[1], cohort_totals[2], cohort_totals[3], cohort_totals[4]),
  yend = y
)

## Secondary running-total labels at each landing point.
running_total_labels <- tibble(
  x = c(1.5, 2.5, 3.5, 4.5),
  y = c(1000, cohort_totals[1], cohort_totals[2], cohort_totals[3]) + 30,
  label = comma(c(1000, cohort_totals[1], cohort_totals[2], cohort_totals[3]))
)

## Plain-language subtext under each phase name
stage_labels <- c(
  "Start"      = "Start",
  "Phase I"    = "Phase I\nSafety",
  "Phase II"   = "Phase II\nProof of concept",
  "Phase III"  = "Phase III\nConfirmation",
  "NDA/BLA"    = "NDA/BLA\nFDA review",
  "Approved"   = "Approved"
)[as.character(waterfall_data$stage)]


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    total  = "#2B2B2A",  
    loss   = "gray70",   
    accent = "#722F37"   
  )
)

### |-  titles and caption ----
title_text <- "The stage with the lowest odds isn't FDA review"

subtitle_text <- str_glue("Of 1,000 experimental drugs entering Phase I trials, more are lost during<br>","
                          Phase II proof-of-concept testing than at any other stage of development")

annotation_text <- "Only 28.9% advance beyond Phase II \u2014 by comparison,<br>90.6% of submitted applications receive approval"

caption_text <- create_swd_caption(
  year = 2026,
  month = "Aug",
  source_text = "BIO, Informa Pharma Intelligence & QLS Advisors, Clinical Development Success Rates 2011\u20132020 (Feb 2021).<br>Figures show a **hypothetical cohort** of 1,000 Phase I programs computed by applying published industry-wide transition rates \u2014 not a single tracked group of drugs."
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(lineheight = 1.1),
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold", size = rel(1.6), family = fonts$title_1,
      margin = margin(b = 8), fonts$title_1, color = clrs$title
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, lineheight = 1.15,
      margin = margin(b = 16), color = clrs$subtitle
    ),
    plot.caption = element_textbox_simple(
      size = rel(0.45), family = fonts$caption, color = alpha(clrs$caption, 0.8),
      margin = margin(t = 10, b = 8)
    )
  )
)

theme_set(weekly_theme)

### |-  plot ----
p <- ggplot(waterfall_data) +
  geom_segment(
    data = connector_data,
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "gray80", linewidth = 0.3
  ) +
  geom_rect(
    aes(
      xmin = x_pos - 0.4, xmax = x_pos + 0.4,
      ymin = ymin, ymax = ymax,
      fill = case_when(
        bar_type == "total" ~ "total",
        is_accent ~ "accent",
        TRUE ~ "loss"
      )
    ),
    color = NA
  ) +
  scale_fill_manual(
    values = c(total = clrs$palette$total, loss = clrs$palette$loss, accent = clrs$palette$accent),
    guide = "none"
  ) +
  geom_text(
    data = waterfall_data |> filter(stage != "NDA/BLA"),
    aes(x = x_pos, y = (ymin + ymax) / 2, label = bar_label),
    color = "white", fontface = "bold", size = 4.2, family = fonts$text
  ) +
  geom_text(
    data = waterfall_data |> filter(stage == "NDA/BLA"),
    aes(x = x_pos, y = ymax + 45, label = bar_label),
    color = "gray30", fontface = "bold", size = 4.2, family = fonts$text,
    vjust = 0
  ) +
  geom_text(
    data = running_total_labels,
    aes(x = x, y = y, label = label),
    color = "gray50", size = 3, family = fonts$text
  ) +
  annotate(
    "richtext",
    x = 4.5, y = 700,
    label = annotation_text,
    fill = NA, label.color = NA,
    color = clrs$palette$accent, size = 3.6, family = fonts$text, fontface = "bold",
    hjust = 0.5
  ) +
  scale_x_continuous(
    breaks = waterfall_data$x_pos,
    labels = stage_labels,
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = NULL
  ) +
  canvas(width = 11, height = 6.5, units = "in", dpi = 300)

p

### |-  save ----
save_ggplot(
  plot = p,
  file = "2026/08_Aug/img/08_Aug.png", 
  width = 11, height = 6.5
)


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
# ─ Session info ──────────────────────────────────────────────────────────
#  setting  value
#  version  R version 4.6.1 (2026-06-24)
#  os       macOS Tahoe 26.5.2
#  system   aarch64, darwin23
#  ui       RStudio
#  language (EN)
#  collate  en_US.UTF-8
#  ctype    en_US.UTF-8
#  tz       America/New_York
#  date     2026-08-01
#  rstudio  2026.07.1+147 Pacific Dogwood (desktop)
#  pandoc   NA
#  quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────
#  ! package      * version date (UTC) lib source
#    base         * 4.6.1   2026-06-25 [?] local
#    cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
#    commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
#  P compiler       4.6.1   2026-06-25 [1] local
#    curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
#  P datasets     * 4.6.1   2026-06-25 [1] local
#    dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
#    farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
#    forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
#    generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
#    ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
#    ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.6.0)
#    ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
#    glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
#  P graphics     * 4.6.1   2026-06-25 [1] local
#  P grDevices    * 4.6.1   2026-06-25 [1] local
#  P grid           4.6.1   2026-06-25 [1] local
#    gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
#    gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
#    here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
#    hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
#    janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
#    jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
#    labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
#    lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
#    litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
#    lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
#    magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
#    markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
#  P methods      * 4.6.1   2026-06-25 [1] local
#    pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
#    patchwork    * 1.3.2   2025-08-25 [1] CRAN (R 4.6.0)
#    pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
#    pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
#    purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
#    R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
#    ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
#    RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
#    Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
#    readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
#    rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
#    rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
#    rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
#    S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
#    scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
#    sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
#    showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
#    showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
#    snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
#  P stats        * 4.6.1   2026-06-25 [1] local
#    stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
#    stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
#    sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
#    systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
#    textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
#    tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
#    tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
#    tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
#    tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
#    timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
#  P tools          4.6.1   2026-06-25 [1] local
#    tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
#  P utils        * 4.6.1   2026-06-25 [1] local
#    vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
#    withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
#    xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
#    xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
#  [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
#  * ── Packages attached to the search path.
#  P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────

