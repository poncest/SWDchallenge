
## Challenge: #SWDchallenge 2025-- Nov Exercise
## Topic:     display partial data appropriately
## Data:      Storytelling with data: before & after, Ch 11, Exercise 6.1
## Author:    Steven Ponce
## Date:      2025-11-10


## 0. DATA SOURCE ----
#' 
#' This exercise is based on Storytelling with data: before & after, Ch 11, Exercise 6.1
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1ZX2MMENdwiwHvJsj34pr_6_FFBqz1KYc/edit?usp=drive_web&ouid=100721939261820638314&rtpof=true


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue        # Interpreted String Literals
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- readxl::read_excel("2025/Ex_061/EX061 - display partial data.xlsx", 
                               sheet = "RAF", 
                               range = "A1:G22",
                               trim_ws = TRUE)


## 3. EXAMINE THE DATA ----
glimpse(raw_data)

# The data has 3 sections:
# - Row 1: Total accounts (annual)
# - Rows 3-15: BY MONTH data
# - Rows 17-21: BY QUARTER data


## 4. TIDY ----

# Helper - Consistent thousands formatter
fmt_k <- label_number(accuracy = 1, suffix = "K", big.mark = ",")

### |- Annual data ----
annual_data <- raw_data |>
  slice(1) |>
  select(-1) |>
  pivot_longer(
    cols = everything(),
    names_to = "date_col",
    values_to = "accounts"
  ) |>
  mutate(
    date = as.Date(as.numeric(date_col), origin = "1899-12-30"),
    year = year(date),
    month_end = month(date),
    accounts_k = accounts,
    # Flag partial year (2025 ends in September, not December)
    is_partial = (year == 2025 & month_end == 9),
    period_label = if_else(is_partial,
      "Jan-Sep 2025\n(partial year)",
      as.character(year)
    ),
    period_type = if_else(is_partial, "Partial", "Full")
  ) |>
  select(date, year, month_end, accounts, accounts_k, is_partial, period_label, period_type)

### |- Monthly data ----
monthly_data <- raw_data |>
  slice(4:15) |>
  rename(month = 1) |>
  pivot_longer(
    cols = -month,
    names_to = "date_col",
    values_to = "accounts"
  ) |>
  mutate(
    date = as.Date(as.numeric(date_col), origin = "1899-12-30"),
    year = year(date),
    month_num = match(month, toupper(month.abb)),
    month_date = make_date(year, month_num, 1),
    accounts_k = accounts,
    is_partial_year = (year == 2025)
  ) |>
  filter(!is.na(accounts)) |>
  select(month, month_num, date, year, month_date, accounts, accounts_k, is_partial_year)

### |- Calculate comparison metrics ----
ytd_2024 <- sum(monthly_data$accounts_k[monthly_data$year == 2024 & monthly_data$month_num <= 9])
ytd_2025 <- annual_data$accounts_k[annual_data$year == 2025]
growth_rate <- ((ytd_2025 - ytd_2024) / ytd_2024) * 100

### |- create comparison data ----
comparison_data <- bind_rows(
  # Misleading comparison (full year vs partial)
  annual_data |>
    filter(year %in% c(2024, 2025)) |>
    mutate(comparison_type = "Misleading: Full Year vs Partial Year"),

  # Fair comparison (YTD vs YTD)
  tibble(
    year = c(2024, 2025),
    accounts_k = c(ytd_2024, ytd_2025),
    is_partial = c(FALSE, FALSE),
    comparison_type = "Fair: Jan-Sep vs Jan-Sep"
  )
) |>
  mutate(
    year_label = as.character(year),
    comparison_type = factor(comparison_type,
      levels = c(
        "Misleading: Full Year vs Partial Year",
        "Fair: Jan-Sep vs Jan-Sep"
      )
    )
  )

### |- P1: X labels: make 2025 explicit as Jan–Sep ---
x_labs_p1 <- annual_data |>
  distinct(year, is_partial) |>
  arrange(year) |>
  mutate(lbl = if_else(is_partial & year == 2025, "2025\n(Jan–Sep)", as.character(year))) |>
  pull(lbl)

### |- P2: panel labels + partial-year stamp ---
comparison_labels <- c(
  "Misleading: Full Year vs Partial Year" = "Unequal periods",
  "Fair: Jan-Sep vs Jan-Sep"              = "Equal periods"
)

partial_tag_df <- comparison_data |>
  filter(comparison_type == "Misleading: Full Year vs Partial Year", year == 2025) |>
  mutate(tag = "(partial year)")


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    partial = "#E08214",
    full = "#8073AC",
    reference = "#1976D2",
    misleading = "#9E9E9E",
    fair = "#E08214"
  )
)

### |-  titles and caption ----
title_p1 <- "2025 Refer-A-Friend Performance Beats Last Year"
subtitle_p1 <- "Fair comparison (Jan-Sep vs Jan-Sep) shows 18% growth, not decline"

title_p2 <- "Why Fair Comparison Matters"
subtitle_p2 <- "Same data, different story — Partial data requires equal comparison periods"

# Create caption
caption_text <- create_swd_exe_caption(
  year = 2025,
  month = "Nov",
  source_text =  "Storytelling with data: before & after, Exercise 6.1 (Chapter 11)"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Text styling 
    plot.title = element_text(
      face = "bold", family = fonts$title, color = colors$title, 
      size = rel(1.65), margin = margin(b = 10)
      ),
    plot.subtitle = element_text(
      face = "italic",family = fonts$subtitle, color = colors$text, 
      size = rel(0.95), margin = margin(b = 20)
      ),
    plot.caption = element_markdown(
      size = rel(0.65), family = fonts$caption, color = colors$caption,
      hjust  = 0, margin = margin(t = 20), lineheight = 1.4,
    ),
    
    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.9)),
    axis.text = element_text(color = colors$text, size = rel(0.8)),
    
    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.1),
    
    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1 plot ----
p1 <-
  annual_data |>
  ggplot(aes(x = factor(year), y = accounts_k)) +
  # Geoms
  geom_col(aes(fill = is_partial), width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = fmt_k(accounts_k)),
    vjust = -0.5, size = 3.8, family = fonts$text,
    color = "gray20", fontface = "bold"
  ) +
  geom_hline(
    yintercept = ytd_2024, linetype = "dashed",
    color = colors$palette$reference, linewidth = 0.85
  ) +
  geom_richtext(
    data = tibble(x = 1.25, y = ytd_2024),
    aes(
      x = x, y = y,
      label = glue("<b>2024 Jan–Sep: {fmt_k(ytd_2024)}</b><br/><span style='font-size:9pt'>(comparison baseline)</span>")
    ),
    fill = alpha("white", 0.9), label.color = NA,
    label.padding = unit(c(2, 4, 2, 4), "pt"),
    hjust = 0, vjust = -0.4, family = fonts$text, color = colors$palette$reference
  ) +
  geom_richtext(
    data = tibble(x = 6, y = ytd_2025 + 25),
    aes(
      x = x, y = y,
      label = glue("<b>+{round(growth_rate, 0)}% growth</b><br/><span style='font-size:9pt'>vs 2024 YTD</span>")
    ),
    fill = NA, label.color = NA,
    hjust = 0.5, vjust = 0, family = fonts$title, color = colors$palette$partial
  ) +
  # Scales
  scale_fill_manual(values = c("FALSE" = colors$palette$full, "TRUE" = colors$palette$partial)) +
  scale_y_continuous(labels = fmt_k, limits = c(0, 560), expand = expansion(mult = c(0, 0.06))) +
  scale_x_discrete(labels = x_labs_p1) +
  # Labs
  labs(
    title = title_p1,
    subtitle = subtitle_p1,
    x = NULL,
    y = "New Accounts (thousands)",
    caption = caption_text
  ) +
  # Theme
  theme(plot.margin = margin(t = 16, r = 15, b = 10, l = 15))

### |- P2 plot ----
p2 <-
  comparison_data |>
  ggplot(aes(x = year_label, y = accounts_k)) +
  # Geoms
  geom_col(aes(fill = comparison_type), width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = fmt_k(accounts_k)),
    vjust = -0.5, size = 3.8, family = fonts$text,
    color = "gray20", fontface = "bold"
  ) +
  geom_text(
    data = partial_tag_df,
    aes(x = year_label, y = accounts_k + 58, label = tag),
    family = fonts$text, size = 3.1, color = "gray40", fontface = "italic"
  ) +
  geom_segment(
    data = comparison_data |>
      group_by(comparison_type) |>
      filter(n() == 2) |>
      summarise(
        x = 1.5,
        y_start = accounts_k[1],
        y_end = accounts_k[2],
        .groups = "drop"
      ),
    aes(x = x, xend = x, y = y_start, yend = y_end),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "gray30", linewidth = 0.7
  ) +
  geom_text(
    data = comparison_data |>
      group_by(comparison_type) |>
      filter(n() == 2) |>
      summarise(
        x = 1.5,
        y = mean(c(accounts_k[1], accounts_k[2])),
        pct_change = ((accounts_k[2] - accounts_k[1]) / accounts_k[1]) * 100,
        .groups = "drop"
      ),
    aes(
      x = x + 0.15, y = y,
      label = paste0(ifelse(pct_change > 0, "+", ""), round(pct_change, 0), "%")
    ),
    size = 3.8, family = fonts$text, fontface = "bold",
    color = "gray20", hjust = 0
  ) +
  # Facets
  facet_wrap(~comparison_type,
    scales = "free_x",
    labeller = as_labeller(comparison_labels)
  ) +
  # Scales
  scale_fill_manual(values = c(
    "Misleading: Full Year vs Partial Year" = colors$palette$misleading,
    "Fair: Jan-Sep vs Jan-Sep"              = colors$palette$fair
  )) +
  scale_y_continuous(labels = fmt_k, limits = c(0, 560), expand = expansion(mult = c(0, 0.06))) +
  # Labs
  labs(
    title = title_p2,
    subtitle = "Same data, different story — compare equal periods to avoid false declines",
    x = NULL,
    y = "New Accounts (thousands)",
    caption = caption_text
  ) +
  # Theme
  theme(
    strip.text = element_text(
      size = rel(1), face = "bold", hjust = 0,
      family = fonts$title, margin = margin(b = 10)
    ),
    strip.background = element_blank(),
    panel.spacing = unit(2.5, "lines")
  )


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #TidyTuesday projects. The core analysis logic
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

# ─ Session info ──────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-10
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggplot2     * 3.5.2    2025-04-09 [?] CRAN (R 4.4.3)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here        * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl        1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P rematch       2.0.0    2023-08-30 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P styler        1.11.0   2025-10-13 [?] CRAN (R 4.4.3)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.1.0    2024-05-15 [?] CRAN (R 4.4.0)
# P textshaping   0.4.0    2024-05-24 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# xfun          0.50     2025-01-07 [1] CRAN (R 4.4.2)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# yaml          2.3.10   2024-07-26 [1] CRAN (R 4.4.2)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────
# > 
