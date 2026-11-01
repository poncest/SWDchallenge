## Challenge: #SWDchallenge 2026
## Topic:     NOV 2026 | tell a story of change
## Author:    Steven Ponce
## Date:      2026-11-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   American Time Use Survey: ATUS 2003–2025 Activity summary file (zip) (15.8 MB) The Activity summary file contains information 
##   about the total time, each ATUS respondent spent doing each activity on the diary day. Because Activity codes have changed somewhat  
##   between 2003 and the present, this file uses activity codes that appear in the 2003–2025 ATUS Coding Lexicon (PDF)
##   https://www.bls.gov/tus/data/datafiles-0325.htm?utm_source=chatgpt.com


## REPRODUCIBILITY ----
##
## Original source:
##   U.S. Bureau of Labor Statistics
##   American Time Use Survey (ATUS), 2003–2025
##   https://www.bls.gov/tus/data/datafiles-0325.htm
##
## Required data:
##   ATUS 2003–2025 Activity summary file (ZIP)
##   Extract: atussum_0325.dat
##
## The raw data file is excluded from GitHub due to its size.
## Download and extract it to:
##   2026/11_Nov/atussum_0325.dat
##
## Analysis:
##   Population: U.S. adults ages 25–64
##   Periods: 2003–07 and 2021–25
##   Excluded year: 2020
##   Survey weight: TUFNWGTP
##
## Household activities:
##   Sum of ATUS activity columns beginning with "t02".
##
## Gender gap:
##   Weighted mean minutes for women minus weighted mean
##   minutes for men, separately by partnership group.
##
## Statistical uncertainty:
##   The caption CI is a historical validated result, NOT computed here.
##   For unadjusted replicate-weight estimates, see atus_unadjusted_replication.R.
##   For exact adjusted analysis and original decision gate, the original
##   atus_final_gate.py and model specification must also be published.
##   Full instructions and limitations: REPRODUCIBILITY.md.
##
## Interpretation:
##   The data describe individual survey respondents,
##   not matched couples. No causal claims are made.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, scales, glue, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure settings ----
fig_w   <- 11
fig_h   <- 6.5
fig_dpi <- 320

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
text_family  <- fonts$text
caption_family <- fonts$caption


## 2. READ IN THE DATA ----

data_path <- here::here("2026", "11_Nov", "atussum_0325.dat")  

atus_raw <- read_csv(
  data_path,
  col_select = c(TUCASEID, TESEX, TEAGE, TRSPPRES, TUYEAR, TUFNWGTP, starts_with("t02")),
  col_types = cols(TUCASEID = col_character(), .default = col_double())
)


## 3. EXAMINE THE DATA ----
glimpse(atus_raw |> select(TUCASEID:TUFNWGTP))
atus_raw |> count(TUYEAR)
atus_raw |> count(TRSPPRES)


## 4. TIDY ----

### |- analysis frame: ages 25-64, 2020 excluded, two pooled periods ----
hh_df <- atus_raw |>
  mutate(hh_min = rowSums(pick(starts_with("t02")))) |>
  filter(between(TEAGE, 25, 64), TUYEAR != 2020) |>
  mutate(
    period = case_when(
      between(TUYEAR, 2003, 2007) ~ "2003–07",
      between(TUYEAR, 2021, 2025) ~ "2021–25"
    ),
    group = case_match(TRSPPRES, 1 ~ "Married adults", 3 ~ "Adults without a partner"),
    sex = if_else(TESEX == 2, "women", "men")
  ) |>
  filter(!is.na(period), !is.na(group))

### |- weighted means by group x period x sex ----
ribbon_df <- hh_df |>
  summarise(minutes = weighted.mean(hh_min, TUFNWGTP), n = n(), .by = c(group, period, sex)) |>
  mutate(
    group = factor(group, levels = c("Married adults", "Adults without a partner")),
    x = if_else(period == "2003–07", 1, 2)
  )

gap_df <- ribbon_df |>
  select(group, period, x, sex, minutes) |>
  pivot_wider(names_from = sex, values_from = minutes) |>
  mutate(gap = women - men)

get_val <- function(g, p, col) {
  gap_df |>
    filter(group == g, period == p) |>
    pull({{ col }})
}
m_gap_1 <- get_val("Married adults", "2003–07", gap)
m_gap_2 <- get_val("Married adults", "2021–25", gap)
n_gap_1 <- get_val("Adults without a partner", "2003–07", gap)
n_gap_2 <- get_val("Adults without a partner", "2021–25", gap)
m_men_chg <- get_val("Married adults", "2021–25", men) - get_val("Married adults", "2003–07", men)
m_women_chg <- get_val("Married adults", "2021–25", women) - get_val("Married adults", "2003–07", women)
diff_in_changes <- (n_gap_2 - n_gap_1) - (m_gap_2 - m_gap_1)

# Validated using the official BLS replicate-weight analysis.
# This script reproduces the descriptive chart, not the CI.
# Retain the separate statistical decision record for full replication.
did_ci <- c(lo = 2.8, hi = 24.3)


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    women = "#2D3142",
    men = "#7A8399",
    band = "#DDD7CB",
    accent = "#722F37",
    soft = "#6B7080",
    paper = "#F4F1EA"
  )
)
clrs <- colors$palette
fallback <- c(
  women = "#2D3142", men = "#7A8399", band = "#DDD7CB", accent = "#722F37",
  soft = "#6B7080", paper = "#F4F1EA"
)
for (k in names(fallback)) if (is.null(clrs[[k]])) clrs[[k]] <- fallback[[k]]

### |- titles and caption ----
title_text <- "The gender gap in household work narrowed more among married adults"

subtitle_text <- "Average daily minutes of household activities reported by U.S. adults ages 25–64"

note_text <- glue(
  "Household activities: housework, food preparation, lawn and garden, household management and similar. ",
  "ATUS diaries record individual respondents, not couples. Married = spouse in the household; without a partner = ",
  "no spouse or unmarried partner in the household. Cohabiting adults not shown. The gap narrowed {round(diff_in_changes, 1)} ",
  "minutes more among married adults than among adults without a partner (95% CI {did_ci[['lo']]}–{did_ci[['hi']]}, BLS replicate weights). 2020 excluded. ",
  "Values are rounded, so differences may not match exactly.<br>"
)

caption_text <- paste0(
  note_text, "<br>",
  create_swd_caption(
    year = 2026,
    month = "Nov",
    source_text = "U.S. Bureau of Labor Statistics, American Time Use Survey 2003–2025"
  )
)

ann_df <- tibble(
  group = factor(c("Married adults", "Adults without a partner"), levels = levels(ribbon_df$group)),
  label = c(
    glue("Gap narrowed by {round(m_gap_1 - m_gap_2)} minutes a day:\nmen's minutes rose, women's fell"),
    "No statistically detectable narrowing"
  ),
  colour = c(clrs$accent, clrs$soft)
) |>
  left_join(gap_df |> filter(x == 2) |> select(group, men), by = "group") |>
  mutate(y = men - 14)

### |- in-band gap labels ("gap" named once: married panel, 2003-07 side) ----
gap_lab <- gap_df |>
  mutate(
    y = (women + men) / 2,
    label = if_else(group == "Married adults" & x == 1,
      as.character(glue("gap {round(gap)}")), as.character(round(gap))
    ),
    hjust = if_else(x == 1, 0, 1),
    x = if_else(x == 1, x + 0.06, x - 0.06)
  )

end_labels <- ribbon_df |>
  mutate(label = if_else(x == 1, as.character(glue("{sex} {round(minutes)}")), as.character(round(minutes))))

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = clrs$paper, colour = NA),
    panel.background = element_rect(fill = clrs$paper, colour = NA),
    plot.title = element_text(
      family = title_family, size = rel(1.6), colour = clrs$women,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.92), colour = clrs$women,
      lineheight = 1.2, margin = margin(b = 16)
    ),
    plot.caption = element_textbox_simple(
      family = caption_family, size = rel(0.55), colour = clrs$soft,
      lineheight = 1.25, width = unit(1, "npc"), halign = 0,
      margin = margin(t = 14)
    ),
    plot.title.position = "plot", plot.caption.position = "plot",
    strip.text = element_text(
      family = text_family, face = "bold", size = rel(1.0), colour = clrs$women,
      hjust = 0, margin = margin(b = 8)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(family = text_family, colour = clrs$soft, size = rel(0.85)),
    axis.text.x = element_text(family = text_family, colour = clrs$soft, size = rel(0.75)),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E2DDD3", linewidth = 0.3),
    panel.spacing = unit(2.5, "lines"),
    plot.margin = margin(18, 24, 12, 16)
  )
)
theme_set(weekly_theme)

### |- main plot ----

p <- ggplot() +
  # Geoms
  geom_ribbon(data = gap_df, aes(x = x, ymin = men, ymax = women), fill = clrs$band) +
  geom_text(
    data = gap_lab, aes(x, y, label = label, hjust = hjust),
    family = text_family, size = 3.2, colour = clrs$soft
  ) +
  geom_line(data = ribbon_df, aes(x, minutes, group = sex, colour = sex), linewidth = 0.8) +
  geom_point(data = ribbon_df, aes(x, minutes, colour = sex), size = 2.6) +
  geom_text(
    data = filter(end_labels, x == 1), aes(x - 0.06, minutes, label = label),
    hjust = 1, family = text_family, size = 3.4, colour = clrs$women
  ) +
  geom_text(
    data = filter(end_labels, x == 2), aes(x + 0.06, minutes, label = label),
    hjust = 0, family = text_family, size = 3.4, colour = clrs$women
  ) +
  # Annotations
  geom_text(
    data = ann_df[1, ], aes(x = 1.5, y = y, label = label), colour = clrs$accent,
    family = text_family, size = 3.5, fontface = "bold", lineheight = 1.0, vjust = 1
  ) +
  geom_text(
    data = ann_df[2, ], aes(x = 1.5, y = y, label = label), colour = clrs$soft,
    family = text_family, size = 3.5, fontface = "bold", lineheight = 1.0, vjust = 1
  ) +
  # Scales
  scale_colour_manual(values = c(men = clrs$men, women = clrs$women), guide = "none") +
  scale_x_continuous(breaks = 1:2, labels = c("2003–07", "2021–25"), limits = c(0.55, 2.45)) +
  scale_y_continuous(breaks = seq(60, 180, 30)) +
  coord_cartesian(ylim = c(50, 175), clip = "off") +
  facet_wrap(~group) +
  # Labs
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text, y = NULL)

### |- preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in", dpi = fig_dpi)


## 6. SAVE ----
save_ggplot(
  plot = p,
  file = "2026/11_Nov/img/11_Nov.png",
  width = fig_w, height = fig_h
)


# 7. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all #SWDchallenge projects. The core analysis logic
# uses only standard tidyverse and ggbump functions.
#
# R/utils/fonts.R
#    - setup_fonts()
#    - get_font_families()
#
# R/utils/social_icons.R
#    - create_swd_caption()
#
# R/themes/base_theme.R
#    - create_base_theme()
#    - extend_weekly_theme()
#    - get_theme_colors()
#
# View source:
# https://github.com/poncest/SWDchallenge/tree/main/R


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-01
# rstudio  2026.10.1+239 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggbump       * 0.1.0   2020-04-24 [1] CRAN (R 4.6.1)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
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
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
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
# styler         1.11.0  2025-10-13 [1] CRAN (R 4.6.0)
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
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 
