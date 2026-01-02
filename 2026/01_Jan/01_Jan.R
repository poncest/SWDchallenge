## Challenge: #SWDchallenge 2026 -- Jan
## Topic:     plot partial information
## Author:    Steven Ponce
## Date:      2026-01-05

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
## or view source code at: https://github.com/poncest/SWDchallenge

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  readr,
  tidyr,
  stringr,
  ggplot2,
  ggtext,
  showtext,
  janitor,
  scales,
  glue
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df <- read_csv(
  here::here("2026/01_Jan/witcher_story_progression.csv"),
  show_col_types = FALSE
)


## 3. EXAMINE THE DATA ----
glimpse(df)
skimr::skim_without_charts(df) 


## 4. TIDY ----
witcher_df <- df |>
  clean_names() |>
  mutate(
    milestone = row_number(),
    milestone_label = case_when(
      milestone == 1 ~ "Tutorial\nComplete",
      milestone == 2 ~ "First Major\nQuest",
      milestone == 3 ~ "Mid-Game\nQuest #1",
      milestone == 4 ~ "Mid-Game\nQuest #2",
      milestone == 5 ~ "Mid-Game\nQuest #3",
      milestone == 6 ~ "Late-Game\nQuest",
      milestone == 7 ~ "Near\nEnd",
      milestone == 8 ~ "Game\nComplete",
      TRUE ~ achievement_name
    ),
    reached = pct_unlocked / 100,
    not_yet = 1 - reached
  )

plot_df <- witcher_df |>
  select(milestone, milestone_label, achievement_name, reached, not_yet) |>
  pivot_longer(
    cols = c(reached, not_yet),
    names_to = "status",
    values_to = "share"
  ) |>
  mutate(
    status = factor(
      status,
      levels = c("reached", "not_yet"),
      labels = c("Reached this milestone", "Did not reach (partial journey)")
    )
  )

start_pct <- witcher_df |> slice(1) |> pull(reached)
finish_pct <- witcher_df |> slice(8) |> pull(reached)
drop_pp <- (start_pct - finish_pct) * 100

  
## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_reached = "#473472",   
    col_partial = "#D3D3D3",   
    col_white   = "white"
  )
)

### |-  titles and caption ----
title_text <- "60% started Geralt's journey. Only 22% finished it."

subtitle_text <- glue(
  "Steam achievement unlock rates for <i>The Witcher 3: Wild Hunt</i> story milestones (snapshot). ",
  "Each bar represents <b>100% of players</b>.<br>",
  "<span style='color:#473472'><b>Purple = reached this milestone</b></span> | ",
  "<span style='color:#999999'><b>Gray = partial journey</b></span>. ",
  "Later bars look 'incomplete' because many players haven't reached them yet—",
  "not because data is missing."
)

caption_text <- create_swd_caption(
  year = 2026,
  month = "Jan",
  source_text = "Steam Community achievement statistics (The Witcher 3: Wild Hunt, appID 292030)"
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
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Axes
    axis.title.x =  element_text(size = rel(0.9), color = "gray30", margin = margin(t = 20)),
    axis.title.y = element_text(size = rel(0.9), color = "gray30",  margin = margin(r = 20)),
    axis.text = element_text(color = "gray30"),
    # axis.text.y = element_text(size = rel(0.95)),
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
    legend.position = "right",
    legend.title = element_text(
      family = fonts$subtitle,
      color = colors$palette$text, size = rel(0.8), face = "bold"
    ),
    legend.text = element_text(
      family = fonts$tsubtitle,
      color = colors$palette$text, size = rel(0.7)
    ),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 10),
    
    # Plot margin
    plot.margin = margin(10, 20, 10, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- main plot ----
ggplot(plot_df, aes(x = milestone, y = share, fill = status)) +

  # Geoms
  geom_col(width = 0.78, color = "grey55", linewidth = 0.4) +
  geom_label(
    data = witcher_df |> filter(milestone == 1),
    aes(x = milestone, y = 0.42, label = percent(reached, accuracy = 0.1)),
    inherit.aes = FALSE,
    fill = colors$palette$col_reached,
    color = "white",
    label.size = 0,
    size = 4,
    fontface = "bold",
    label.padding = unit(0.25, "lines")
  ) +
  geom_label(
    data = witcher_df |> filter(milestone == 8),
    aes(x = milestone, y = 0.82, label = percent(reached, accuracy = 0.1)),
    inherit.aes = FALSE,
    fill = colors$palette$col_reached,
    color = "white",
    label.size = 0,
    size = 4,
    fontface = "bold",
    label.padding = unit(0.65, "lines")
  ) +

  # Annotate
  annotate(
    "text",
    x = 1, y = 1.045, label = "Started",
    color = colors$palette$col_reached, fontface = "bold", size = 3.9, hjust = 0.5
  ) +
  annotate(
    "text",
    x = 8, y = 1.045, label = "Finished",
    color = colors$palette$col_reached, fontface = "bold", size = 3.9, hjust = 0.5
  ) +
  annotate(
    "segment",
    x = 1.25, xend = 7.75, y = 1.02, yend = 1.02,
    linewidth = 0.35, color = "grey55",
    arrow = arrow(ends = "both", type = "closed", length = unit(0.12, "inches"))
  ) +
  annotate(
    "text",
    x = 4.5, y = 1.055,
    label = glue("{round(drop_pp, 1)} percentage point drop"),
    size = 3.6, color = "grey45", fontface = "italic", hjust = 0.5
  ) +
  annotate(
    "label",
    x = 8.6, y = 0.63,
    label = "Partial journeys (not missing data)\nMost players stopped before finishing.",
    hjust = 0, vjust = 1,
    size = 3.4, color = "grey20",
    fill = "white", label.size = 0.25, label.r = unit(0.18, "lines"),
    label.padding = unit(0.35, "lines")
  ) +

  # Scales
  scale_x_continuous(
    breaks = 1:8,
    labels = witcher_df$milestone_label,
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.08),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = c(
      "Reached this milestone" = colors$palette$col_reached,
      "Did not reach (partial journey)" = colors$palette$col_partial
    ),
    breaks = c("Reached this milestone", "Did not reach (partial journey)")
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "Story progression milestones",
    y = "Share of players",
    fill = NULL,
    caption = caption_text
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
      size = rel(0.65),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    ),
    legend.position.inside = c(0.12, 0.92),
    legend.justification = c(0, 1),
    legend.key.spacing.y = unit(0.3, "cm"),
    legend.key.height = unit(0.8, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.text = element_text(size = rel(0.7)),
    legend.margin = margin(r = 10),
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

# ─ Session info ──────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2026-01-01
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────
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
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate     1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr         1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
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
# P tibble        3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
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
# ─────────────────────────────────────────────
# > 