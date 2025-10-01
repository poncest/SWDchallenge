# Challenge:  #SWDchallenge 2025 -- October
## Topic:     avoid the spaghetti graph
## Author:    Steven Ponce
## Date:      2025-10-01

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
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- readxl::read_excel(
  '2025/10_Oct/SWDchallenge OCT2025.xlsx',
  range = "C7:I12") |> 
  clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)

## 4. TIDY ----
arrow_data <- raw_data |>
  rename(category = x1) |>
  select(category, y2020 = x2020, y2025 = x2025) |>
  mutate(
    category = str_to_title(category),
    change = y2025 - y2020,
    change_pct = change,
    direction = if_else(change > 0, "Growth", "Decline")
  ) |>
  arrange(desc(y2025))

arrow_data <- arrow_data |>
  mutate(
    category = factor(category, levels = rev(category)),
    lab_2020 = label_percent(accuracy = 1)(y2020),
    lab_2025 = label_percent(accuracy = 1)(y2025),
    lab_delta = if_else(change >= 0,
      paste0("+", label_percent(accuracy = 1)(change)),
      label_percent(accuracy = 1)(change)
    ),
    x_mid = (y2020 + y2025) / 2,
    hjust_2020 = if_else(y2020 <= y2025, 1.15, -0.15),
    hjust_2025 = if_else(y2020 <= y2025, -0.15, 1.15)
  )

median_2025 <- median(arrow_data$y2025, na.rm = TRUE)
n_cats <- nrow(arrow_data)
max_x <- max(arrow_data$y2020, arrow_data$y2025, na.rm = TRUE)


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    `TRUE` = "#1976D2",   
    `FALSE` = "#F57C00"
  )
)

### |-  titles and caption ----
title_text <- str_glue("Health Funding Surges While Education Slips")
subtitle_text <- str_glue(
  "Funder priorities, 2020 -> 2025 (dashed = 2025 median)\n",
  "Percentages can exceed 100% because funders choose multiple categories • Data self-reported by funders"
  )

# Create caption
caption_text <- create_swd_caption(
  year = 2025,
  month = "Oct",
  source_text =  "Storytelling with Data: A Data Visualization Guide for Business Professionals"
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
    plot.title = element_text(face = "bold", family = fonts$title, color = colors$title, size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    
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

# |- plot ----
ggplot(arrow_data, aes(y = category)) +

  # Geoms
  geom_vline(
    xintercept = median_2025, linetype = "dashed",
    color = "gray35", linewidth = 0.7
  ) +

  # median tag
  annotate(
    "label",
    x = median_2025, y = n_cats + 0.35,
    label = paste0("2025 median: ", label_percent(accuracy = 1)(median_2025)),
    size = 3.2, fontface = "bold",
    label.size = 0, fill = "white", alpha = 0.95, color = "gray15"
  ) +

  # arrows
  geom_segment(
    aes(x = y2020, xend = y2025, yend = category, color = change > 0),
    linewidth = 1.4, lineend = "round",
    arrow = arrow(length = unit(0.28, "cm"), type = "closed")
  ) +

  # arrows labels
  geom_text(aes(x = y2020, label = lab_2020, hjust = hjust_2020),
    size = 3.2, color = "gray35", fontface = "bold"
  ) +
  geom_text(aes(x = y2025, label = lab_2025, hjust = hjust_2025, color = change > 0),
    size = 3.2, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(aes(x = x_mid, label = lab_delta, color = change > 0),
    vjust = -0.9, size = 3.3, fontface = "bold", show.legend = FALSE
  ) +

  # Scales
  scale_x_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, max_x + 0.12),
    breaks = seq(0, 0.90, by = 0.10),
    expand = expansion(mult = c(0.01, 0.07))
  ) +
  scale_y_discrete() +
  scale_color_manual(values = colors$palette) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "Percent of funders", y = NULL,
    caption = caption_text
  ) +

  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.4),
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.2,
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    )
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-01
# rstudio  2025.09.0+387 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────
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
# P styler        1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
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
# ─────────────────────────────────────────────────────
# >
