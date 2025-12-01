## Challenge: #SWDchallenge 2025 -- Dec
## Topic:     when less is better
## Author:    Steven Ponce
## Date:      2025-12-01

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
  glue,        # Interpreted String Literals
  Lahman       # Sean 'Lahman' Baseball Database
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
batting_raw <- Lahman::Batting
people_raw  <- Lahman::People

### |- PED-linked players (Source: ESPN - The Steroids Era) ----
# https://www.espn.com/mlb/topics/_/page/the-steroids-era
# "Of the 10 players [who reached 500 HR between 1998-2009], six -- Barry Bonds, 
# Alex Rodriguez, Mark McGwire, Manny Ramirez, Rafael Palmeiro and Gary Sheffield 
# -- have been linked to PEDs."
# Sammy Sosa added based on leaked 2003 test results

ped_players <- c(
  "Mark McGwire",
  "Barry Bonds",
  "Alex Rodriguez",
  "Sammy Sosa",
  "Manny Ramirez",
  "Rafael Palmeiro",
  "Gary Sheffield"
)


## 3. EXAMINE THE DATA ----
glimpse(batting_raw)
glimpse(people_raw)


## 4. TIDY ----
career_hr <- batting_raw |>
  group_by(playerID) |>
  summarize(
    career_HR = sum(HR, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(career_HR >= 500) |>
  left_join(
    people_raw |> select(playerID, nameFirst, nameLast),
    by = "playerID"
  ) |>
  mutate(player_name = paste(nameFirst, nameLast)) |>
  arrange(desc(career_HR))

cumulative_stats <- batting_raw |>
  filter(playerID %in% career_hr$playerID) |>
  group_by(playerID, yearID) |>
  summarize(
    season_HR = sum(HR, na.rm = TRUE),
    season_G  = sum(G, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  arrange(playerID, yearID) |>
  group_by(playerID) |>
  mutate(
    cumulative_HR = cumsum(season_HR),
    cumulative_G  = cumsum(season_G)
  ) |>
  ungroup()

race_data <- cumulative_stats |>
  filter(cumulative_HR <= 550) |>
  left_join(
    people_raw |> select(playerID, nameFirst, nameLast),
    by = "playerID"
  ) |>
  mutate(
    player_name = paste(nameFirst, nameLast),
    is_ped = player_name %in% ped_players,
    player_category = case_when(
      player_name == "Mark McGwire" ~ "mcgwire",
      player_name == "Babe Ruth" ~ "ruth",
      player_name == "Eddie Murray" ~ "murray",
      TRUE ~ "other"
    )
  )

  
## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    mcgwire = "#BF092F",  
    ruth  = "#132440",  
    murray = "#3B9797", 
    others = "gray75",
    milestone = "gray55",
    grid = "gray90"
  )
)

### |-  titles and caption ----
title_text <- "The Race to 500 Home Runs"

subtitle_text <- str_glue(
  "**McGwire** was fastest (1,688 games) but linked to PED use. **Babe Ruth** holds the fastest clean record at 1,790 games,<br>",
  "while **Eddie Murray** needed 2,971 games.<br><br>",
  "<span style='color:gray50; font-size:10pt;'>7 of 28 club members have been linked to performance-enhancing drugs (1929–2021)</span>"
)

# Create caption
caption_text <- create_swd_caption(
  year = 2025,
  month = "Dec",
  source_text = "500 HR Club trajectories: Lahman Baseball Database • PED links: ESPN “The Steroids Era”"
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
      color = colors$palette$title, margin = margin(b = 10), hjust = 0
    ),
    plot.subtitle = element_text(
      family = fonts$subtitle, lineheight = 1.2,
      color = colors$palette$subtitle, size = rel(0.9), margin = margin(b = 20), hjust = 0
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
      color = colors$palette$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$palette$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 15),

    # Plot margin
    plot.margin = margin(10, 20, 10, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- main plot ----
ggplot(race_data, aes(x = cumulative_G, y = cumulative_HR, group = player_name)) +
  # Geoms
  geom_hline(
    yintercept = 500,
    linetype   = "dashed",
    color      = colors$palette$milestone,
    linewidth  = 0.5
  ) +
  geom_line(
    data = race_data |> filter(player_category == "other"),
    color = colors$palette$others,
    linewidth = 0.5,
    alpha = 0.6
  ) +
  geom_line(
    data = race_data |> filter(player_category == "murray"),
    color = colors$palette$murray,
    linewidth = 1.1
  ) +
  geom_line(
    data = race_data |> filter(player_category == "ruth"),
    color = colors$palette$ruth,
    linewidth = 1.1
  ) +
  geom_line(
    data = race_data |> filter(player_category == "mcgwire"),
    color = colors$palette$mcgwire,
    linewidth = 1.4
  ) +
  # Annotations
  annotate(
    "text",
    x = 1500, y = 545,
    label = "McGwire*\n1,688 games",
    hjust = 0.5, vjust = 0,
    size = 3.6,
    color = colors$palette$mcgwire,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 1820, y = 585,
    label = "Ruth\n1,790 games\n(fastest clean)",
    hjust = 0, vjust = 0.5,
    size = 3.3,
    color = colors$palette$ruth,
    fontface = "bold",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = 3200, y = 515,
    label = "Murray\n2,971 games\n(slowest)",
    hjust = 1, vjust = 0,
    size = 3.3,
    color = colors$palette$murray,
    fontface = "bold",
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = 80, y = 515,
    label = "500 HR milestone",
    hjust = 0, vjust = 0,
    size = 3.1,
    color = colors$palette$milestone
  ) +
  # Scales
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, 3200),
    breaks = seq(0, 3000, 500),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 600),
    breaks = seq(0, 500, 100)
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Games played",
    y        = "Cumulative home runs"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(2),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.3,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.55),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    ),
    axis.title = element_text(size = 10, color = "gray35"),
    axis.text = element_text(size = 9,  color = "gray35")
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

# ─ Session info ───────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-12-01
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
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
# P Lahman      * 13.0-0   2025-09-08 [?] CRAN (R 4.4.3)
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
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────
# > 