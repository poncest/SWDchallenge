
# Challenge:  #SWDchallenge 2025 -- May
## Topic:     compare human and machine
## Author:    Steven Ponce
## Date:      2025-05-02


## Original Chart
# Death from Natural Disasters
# https://data.world/makeovermonday/a-century-of-global-deaths-from-disasters

## Source Data
# Our World in Data via Makeover Monday 2024 week 25
# https://data.world/makeovermonday/a-century-of-global-deaths-from-disasters

##  Article
# Our World in Data, "Natural Disasters: How many people die from disasters, and how are these impacts changing over time?"
# https://ourworldindata.org/natural-disasters


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  skimr,             # Compact and Flexible Summaries of Data
  ggridges,          # Ridgeline Plots in 'ggplot2'
  RColorBrewer,      # ColorBrewer Palettes
  camcorder          # Record Your Plot History
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
deaths_disasters_type_raw <- read_csv("2025/05_May/decadal-deaths-disasters-type.csv") |> 
  clean_names() 


## 3. EXAMINE THE DATA ----
glimpse(deaths_disasters_type_raw)
skim(deaths_disasters_type_raw)
 

## 4. TIDY ----
# Pivot longer
deaths_long <- deaths_disasters_type_raw |>
  pivot_longer(
    cols = starts_with("deaths_"),
    names_to = "disaster_type",
    values_to = "deaths"
  ) |>
  # Clean disaster type names
  mutate(
    disaster_type = str_remove(disaster_type, "deaths_"),
    disaster_type = str_remove(disaster_type, "_decadal"),
    disaster_type = str_replace_all(disaster_type, "_", " "),
    disaster_type = str_to_title(disaster_type)
  )

# Summary by disaster type
disaster_summary <- deaths_long |>
  group_by(disaster_type) |>
  summarize(
    total_deaths = sum(deaths, na.rm = TRUE),
    countries_affected = sum(deaths > 0, na.rm = TRUE),
    max_deaths = max(deaths, na.rm = TRUE), 
    .groups = "drop" 
  ) |>  
  arrange(desc(total_deaths))

# Top 5 disaster types
top5_disasters <- disaster_summary |>
  top_n(5, total_deaths) |>
  pull(disaster_type)

# Plot data
plot_data <- deaths_long |>
  filter(
    deaths > 0,
    year >= 1950,  
    disaster_type %in% top5_disasters
  ) |>
  # Reorder disaster types by total deaths
  mutate(
    disaster_type = factor(
      disaster_type,   
      levels = disaster_summary |> 
        filter(disaster_type %in% top5_disasters) |> 
        arrange(desc(total_deaths)) |> 
        pull(disaster_type)
    ),
    decade = as.factor(year)
  )


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = rev(brewer.pal(8, "Spectral")))

### |-  titles and caption ----
title_text   <- str_glue("Disaster Death Distributions: 1950-2020") 
subtitle_text <- str_glue("How the patterns of mortality from natural disasters changed over time")

# Create caption
caption_text <- create_swd_caption(
    year = 2025,
    month = "May",
    source_text = "Data Source: Our World in Data, 'Death from Natural Disasters'"
  )

# |- fonts ----
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
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
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
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Final Plot  ----
ggplot(plot_data, aes(x = deaths, y = decade, fill = decade, height = after_stat(density))) +
  # Geoms
  geom_density_ridges(
    scale = 3,
    alpha = 0.85,
    rel_min_height = 0.01,
    bandwidth = 0.5,
    color = "white",
    linewidth = 0.2
  ) +
  # Scales
  scale_x_log10(
    # Special transformation with pseudo-log scale starting at 0
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
    labels = c("0", "1", "10", "100", "1K", "10K", "100K", "1M", "10M"),
    expand = c(0.01, 0),
    # Add small offset to avoid log(0) issue
    trans = scales::pseudo_log_trans(base = 10)
  ) +
  scale_fill_manual(values = colors$palette) +
  scale_y_discrete(expand = c(0, 0)) +
  # Facets
  facet_wrap(
    ~disaster_type,
    ncol = 3,
    scales = "fixed"
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Deaths per Decade (log scale)",
    y = NULL,
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.85),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    strip.background = element_rect(fill = "#e0e0e0", color = NA),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(1, "lines"),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-02
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc      0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder    * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli            3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark     1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl           5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# evaluate       1.0.3    2025-01-10 [1] CRAN (R 4.4.2)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.4.2)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggridges     * 0.5.6    2024-01-23 [?] CRAN (R 4.4.0)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here         * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools      0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor      * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# knitr          1.49     2024-11-08 [1] CRAN (R 4.4.2)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown       1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods      * 4.4.0    2024-04-24 [?] local
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# pillar         1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# RColorBrewer * 1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv           1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr           1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# rlang          1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr        * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts    1.1.0    2024-05-15 [?] CRAN (R 4.4.0)
# P textshaping    0.4.0    2024-05-24 [?] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8           1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# xfun           0.50     2025-01-07 [1] CRAN (R 4.4.2)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────
# 
# >