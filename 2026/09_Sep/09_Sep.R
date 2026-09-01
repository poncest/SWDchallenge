## Challenge: #SWDchallenge 2026
## Topic:     SEP 2026 | bump it!
## Author:    Steven Ponce
## Date:      2026-09-01

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Data source:
##   WHO Global Health Estimates 2021: global DALYs by cause, 2000-2021
##   https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/global-health-estimates-leading-causes-of-dalys


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, scales, glue, ggview, ggbump
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

# The workbook was tidied separately in `WHO_GHE_DALY_tidy.R`.
df_tidy <- read_csv(
  here::here("2026/09_Sep/who_ghe_daly_global_tidy.csv"),
  show_col_types = FALSE
) |>
  clean_names()


## 3. EXAMINE THE DATA ----
glimpse(df_tidy)
skimr::skim_without_charts(df_tidy)


## 4. PREPARE THE BUMP-CHART DATA ----

endpoint_causes <- c(
  "Lower respiratory infections", "Diarrhoeal diseases",
  "Preterm birth complications", "Ischaemic heart disease", "Stroke",
  "Tuberculosis", "HIV/AIDS", "Birth asphyxia and birth trauma", "Malaria",
  "Road injury", "Measles", "Chronic obstructive pulmonary disease",
  "Back and neck pain", "Congenital anomalies", "Other neonatal conditions",
  "Cirrhosis of the liver", "Self-harm", "Diabetes mellitus",
  "Depressive disorders", "Interpersonal violence", "COVID-19",
  "Trachea, bronchus, lung cancers", "Other hearing loss", "Falls",
  "Kidney diseases"
)

year_lookup <- tibble(year = c(2000, 2010, 2015, 2019, 2020, 2021), year_index = 1:6)
# x-axis uses year_index (equal categorical spacing), not true year value.
# True temporal spacing was tested and reverted: it made
# COVID's rise correctly abrupt but crowded the 2019-2021 endpoint label
# Caption disclose the six periods are unevenly spaced in reality.

highlight_causes <- tribble(
  ~cause,                 ~story_group,
  "Diabetes mellitus",   "Rising chronic burden",
  "Back and neck pain",  "Rising chronic burden",
  "HIV/AIDS",            "Declining infectious burden",
  "Measles",             "Declining infectious burden",
  "Diarrhoeal diseases", "Declining infectious burden",
  "COVID-19",            "Pandemic disruption"
)

df_bump <- df_tidy |>
  filter(cause %in% endpoint_causes) |>
  group_by(year) |>
  mutate(rank = min_rank(desc(dalys))) |>
  ungroup() |>
  left_join(year_lookup, by = "year") |>
  left_join(highlight_causes, by = "cause") |>
  mutate(
    story_group = factor(
      story_group,
      levels = c("Rising chronic burden", "Declining infectious burden", "Pandemic disruption")
    )
  ) |>
  arrange(cause, year)

df_background <- df_bump |> filter(is.na(story_group))
df_highlight  <- df_bump |> filter(!is.na(story_group))
df_labels <- df_highlight |> filter(year == 2021) |> mutate(label = glue("{cause}  ({rank})"))


## 5. VISUALIZATION ----

clrs <- get_theme_colors(
  palette = list(
    rising     = "#722F37", 
    declining  = "#7A8B92", 
    disruption = "#5C5954", 
    background = "#C7C2B8",
    accent     = "#722F37", 
    neutral    = "#9B968C"  
  )
)

story_colors <- c(
  "Rising chronic burden" = clrs$palette[["rising"]],
  "Declining infectious burden" = clrs$palette[["declining"]],
  "Pandemic disruption" = clrs$palette[["disruption"]]
)

### |- titles and caption ----
title_text <- "Chronic burdens climbed as infectious diseases fell"

subtitle_text <- glue(
  "Among 25 causes that ranked among WHO's leading causes of global disease burden ",
  "in 2000 or 2021, <span style='color:{story_colors[[\"Rising chronic burden\"]]}'><b>diabetes rose from 18th to 7th</b></span>, ",
  "while <span style='color:{story_colors[[\"Declining infectious burden\"]]}'><b>HIV/AIDS fell from 7th to 21st</b></span> ",
  "and measles from 11th to 25th. COVID-19 disrupted the order in 2020-21."
)

caption_text <- paste0(
  create_swd_caption(
    year = 2026,
    month = "Sep",
    source_text = "WHO Global Health Estimates 2021"
  ),
  "<br>DALY = one year of healthy life lost. Ranks are among 25 causes appearing in WHO's top 20 in 2000 or 2021.",
  "<br>Years are unevenly spaced (2000-2021) but plotted at equal intervals."
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(family = fonts$text, size = 9, color = "grey35"),
    axis.text.y = element_text(family = fonts$text, size = 8, color = "grey55"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey91", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      family = fonts$title_1, face = "bold", size = 22, margin = margin(b = 8)
      ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 11, margin = margin(t = 4, b = 20), 
      lineheight = 1.25
    ),
    plot.caption = element_markdown(
      family = fonts$caption, size = 7, color = "grey40", hjust = 0, 
      margin = margin(t = 20, b = 0),  lineheight = 1.15
      ),
    plot.margin = margin(t = 24, r = 130, b = 24, l = 36)
  )
)

theme_set(weekly_theme)

### |- bump chart ----
p <- ggplot(df_bump, aes(x = year_index, y = rank, group = cause)) +
  geom_bump(
    data = df_background,
    color = clrs$palette[["neutral"]],
    alpha = 0.45,
    linewidth = 0.2,
    smooth = 7
  ) +
  geom_bump(
    data = df_highlight,
    aes(color = story_group, linewidth = story_group),
    smooth = 7
  ) +
  geom_point(
    data = df_highlight,
    aes(color = story_group),
    size = 2.4
  ) +
  geom_text(
    data = df_labels,
    aes(x = 6.12, label = label, color = story_group),
    hjust = 0,
    size = 3.15,
    family = fonts$text,
    fontface = "bold"
  ) +
  scale_color_manual(values = story_colors, guide = "none") +
  scale_linewidth_manual(
    values = c(
      "Rising chronic burden" = 1.35,
      "Declining infectious burden" = 1.2,
      "Pandemic disruption" = 1.05
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = year_lookup$year_index,
    labels = year_lookup$year,
    limits = c(0.72, 6.25),
    expand = expansion(mult = 0)
  ) +
  scale_y_reverse(
    breaks = c(1, 5, 10, 15, 20, 25),
    limits = c(25.5, 0.5),
    expand = expansion(mult = 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |- preview ----
p +
  canvas(width = 10, height = 8, units = "in", dpi = 300)

## 6. SAVE ----
save_ggplot(
  plot = p,
  file = "2026/09_Sep/img/09_Sep.png",
  width = 10, height = 8
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

# ─ Session info ─────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-01
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
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
# evaluate       1.0.5   2025-08-27 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# fastmap        1.2.0   2024-05-15 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggbump       * 0.1.0   2020-04-24 [1] CRAN (R 4.6.1)
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
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# knitr          1.51    2025-12-20 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# repr           1.1.7   2024-03-22 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# skimr          2.2.2   2026-01-10 [1] CRAN (R 4.6.0)
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
# utf8           1.2.6   2025-06-08 [1] CRAN (R 4.6.0)
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
# ────────────────────────────────────────────────────────────────────────────────────────
# 
