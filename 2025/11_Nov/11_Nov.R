## Challenge: #SWDchallenge 2025 -- Nov
## Topic:     discover the dot plot
## Author:    Steven Ponce
## Date:      2025-11-01

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
## or view source code at: https://github.com/poncest/SWDchallenge

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  scales,      # Scale Functions for Visualization
  glue         # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
# The data comes from TidyTuesday 2025 week 13
pokemon_raw <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv"
) |>
  clean_names()


## 3. EXAMINE THE DATA ----
glimpse(pokemon_raw)
skimr::skim(pokemon_raw)


## 4. TIDY ----
pokemon_summary <- pokemon_raw |>
  select(pokemon, type_1, speed) |>
  filter(!is.na(speed), !is.na(type_1)) |>
  group_by(type_1) |>
  summarize(
    median_speed = median(speed),
    min_speed = min(speed),
    max_speed = max(speed),
    count = n(),
    .groups = "drop"
  ) |>
  arrange(desc(median_speed)) |>
  mutate(
    type_1 = fct_reorder(type_1, median_speed),
    rank = row_number(desc(median_speed)),
    is_labeled = rank <= 3 | rank > n() - 3,
    label_position = if_else(rank <= 3, "right", "left")
  )

overall_median <- median(pokemon_raw$speed, na.rm = TRUE)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    "dot" = "#8073AC",
    "range" = "gray70",
    "top" = "#E08214",
    "bottom" = "#542788"
  )
)

### |-  titles and caption ----
title_text <- "Pokémon Speed by Type"
subtitle_text <- "Flying types are the fastest, while Fairy types are the slowest"

# Create caption
caption_text <- create_swd_caption(
  year = 2025,
  month = "Nov",
  source_text = "{ pokemon R package }"
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
      face = "bold", family = fonts$title, size = rel(1.4),
      color = colors$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      face = "italic", family = fonts$subtitle, lineheight = 1.2,
      color = colors$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
    ),

    ## Grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

    # Axes
    axis.title = element_text(size = rel(0.9), color = "gray30"),
    axis.text = element_text(color = "gray30"),
    axis.text.y = element_text(size = rel(0.95)),
    axis.ticks = element_blank(),

    # Facets
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(
      face = "bold",
      color = "gray20",
      size = rel(1),
      margin = margin(t = 8, b = 8)
    ),
    panel.spacing = unit(2, "lines"),

    # Legend elements
    legend.position = "plot",
    legend.title = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- main plot ----
ggplot(pokemon_summary, aes(y = type_1)) +
  # Geoms
  geom_vline(
    xintercept = overall_median,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = min_speed, xend = max_speed, yend = type_1),
    color = colors$palette$range,
    linewidth = 2.5,
    alpha = 0.35
  ) +
  geom_point(
    aes(x = min_speed),
    color = colors$palette$range,
    size = 2.5,
    alpha = 0.6,
    shape = 124
  ) +
  geom_point(
    aes(x = max_speed),
    color = colors$palette$range,
    size = 2.5,
    alpha = 0.6,
    shape = 124
  ) +
  geom_point(
    aes(x = median_speed),
    color = colors$palette$dot,
    size = 6.5,
    alpha = 0.9
  ) +
  geom_text(
    data = pokemon_summary |> filter(rank <= 3),
    aes(x = median_speed + 7, label = round(median_speed)),
    hjust = 0,
    size = 4,
    fontface = "bold",
    family = fonts$text,
    color = colors$palette$top
  ) +
  geom_text(
    data = pokemon_summary |> filter(rank > n() - 3),
    aes(x = median_speed - 7, label = round(median_speed)),
    hjust = 1,
    size = 4,
    fontface = "bold",
    family = fonts$text,
    color = colors$palette$bottom
  ) +
  # Scales
  scale_x_continuous(
    breaks = seq(0, 180, 20),
    limits = c(0, 180),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_discrete(
    labels = function(x) {
      counts <- pokemon_summary$count[match(x, pokemon_summary$type_1)]
      glue("{str_to_title(x)} (n={counts})")
    },
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Speed",
    y = "Pokémon Type"
  ) +
  # Annotations
  annotate(
    "text",
    x = overall_median - 3,
    y = 18.5,
    label = "Overall\nMedian",
    size = 2.6,
    color = "gray50",
    lineheight = 0.9,
    hjust = 1,
    fontface = "italic"
  ) +
  annotate(
    "segment",
    x = 135, xend = 165, y = 19, yend = 19,
    color = colors$palette$range,
    linewidth = 2.5,
    alpha = 0.35
  ) +
  annotate(
    "point",
    x = c(135, 165), y = c(19, 19),
    color = colors$palette$range,
    size = 2.5,
    alpha = 0.6,
    shape = 124
  ) +
  annotate(
    "point",
    x = 150, y = 19,
    color = colors$palette$dot,
    size = 6.5,
    alpha = 0.9
  ) +
  annotate(
    "text",
    x = 150, y = 19.5,
    label = "Range (min-max) and median",
    size = 2.6,
    color = "gray40",
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = 0, y = 19.1,
    label = "Top 3 &\nBottom 3 labeled",
    size = 3.6,
    color = "gray40",
    hjust = 0
  ) +
  annotate(
    "text",
    x = 170, y = 19,
    label = "95",
    size = 3.5,
    color = colors$palette$top,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 130, y = 19,
    label = "45",
    size = 3.5,
    color = colors$palette$bottom,
    fontface = "bold"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      family = fonts$subtitle,
      color = "gray40",
      margin = margin(b = 15)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 15)
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-01
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
#
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# evaluate      1.0.3    2025-01-10 [1] CRAN (R 4.4.2)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# fastmap       1.2.0    2024-05-15 [1] CRAN (R 4.4.2)
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
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# knitr         1.49     2024-11-08 [1] CRAN (R 4.4.2)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr         2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
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
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# xfun          0.50     2025-01-07 [1] CRAN (R 4.4.2)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# yaml          2.3.10   2024-07-26 [1] CRAN (R 4.4.2)
#
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
#
# ─────────────────────────────────────────────────────────────────────────────────────
# >
