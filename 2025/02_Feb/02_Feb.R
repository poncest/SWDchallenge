
# Challenge:  #SWDchallenge 2025 -- February
## Topic:     reclaim the streamgraph
## Data:      United States Department of Agriculture
##            National Agricultural Statistics Service
## Author:    Steven Ponce
## Date:      2025-02-01


## 0. DATA SOURCE ----
#' United States Department of Agriculture
#' National Agricultural Statistics Service
#' 
#' Links:
#' https://www.nass.usda.gov/Data_and_Statistics/
#' https://quickstats.nass.usda.gov/results/3A29A7C1-6D8D-347A-908F-89E54126430F
#' 


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
  camcorder,         # Record Your Plot History
  ggstream           # Create Streamplots in 'ggplot2'
) 

### |- figure size ---- 
gg_record( 
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

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
vegatables_raw <- read_csv("2025/02_Feb/NASS - 52F3230D-BDBF-3D09-902C-7125CCE63C9F.csv") |> 
  clean_names() 


## 3. EXAMINE THE DATA ----
glimpse(vegatables_raw)
skim(vegatables_raw)


## 4. TIDY ----
vegatables_clean <- vegatables_raw |>
  # Select only the relevant columns 
  select(year, commodity, value) |>
  # Handle special codes 
  filter(
    value != "(D)",  # Withheld to avoid disclosing data
    value != "(Z)",  # Less than half unit
    value != "(S)",  # Insufficient reports
    value != "(NA)", # Not available
    value != "(X)"   # Not applicable
  ) |>
  mutate(
    value = as.numeric(value),
    # Format commodity names
    commodity = case_when(
      commodity == "SWEET CORN" ~ "Sweet Corn",
      commodity == "POTATOES" ~ "Potatoes",
      commodity == "TOMATOES" ~ "Tomatoes",
      TRUE ~ str_to_title(commodity)
    )
  ) |>
  # Remove any remaining NA values 
  filter(!is.na(value)) |>
  # Group and summarize
  group_by(year, commodity) |>
  summarise(total_acres = sum(value, na.rm = TRUE), .groups = 'drop') |> 
  ungroup()


# Tibble for manual label positions
label_positions <- tibble(
  commodity = c("Potatoes", "Spinach", "Squash", "Sweet Corn", "Tomatoes"),
  # X positions for vertical alignment
  x_position = c(2002, 2005, 2008, 2011, 2013),
  # Label positions - extending beyond the streams
  y_position = c(25000, 35000, 45000, 60000, -60000),  
  # Stream connection points - where the lines should touch the streams
  y_start = c(19000, 10000, 8000, 8000, -30000)    
)


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "Potatoes"   = "#C4A484",   
  "Spinach"    = "#165B33",     
  "Squash"     = "#FFB01F",      
  "Sweet Corn" = "#F7E03D",   
  "Tomatoes"   = "#E41B17"    
  )
)


### |-  titles and caption ----
title_text   <- str_glue("Five Major U.S. Fresh Vegetable Crops (2000-2022)") 
subtitle_text <- str_glue("A streamgraph showing harvested acres across different vegetables")

# Create caption
caption_text <- create_swd_caption(
    year = 2025,
    month = "Feb",
    source_text = "Data Source: USDA Agricultural Statistics Service"
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
    plot.title = element_text(face = "bold", size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis formatting
    axis.title   = element_text(color = colors$text, face = "bold", size = rel(0.72)),
    axis.text    = element_text(color = colors$text, size = rel(0.9)),
    axis.line.x  = element_line(color = "#252525", linewidth = .3),
    axis.ticks.x = element_line(color = colors$text),  

    # Grid customization
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = .4),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(vegatables_clean, 
       aes(x = year, 
           y = total_acres, 
           fill = commodity, 
           group = commodity)) +
  geom_stream(
    type = "mirror",
    bw = 0.85,
    extra_span = 0.2
  ) +
  # Add vertical connecting lines
  geom_segment(
    data = label_positions,
    aes(
      x = x_position,
      y = y_start,
      xend = x_position,
      yend = y_position
    ),
    color = colors$text,
    linewidth = 0.3,
    linetype = "solid"
  ) +
  # Add points at stream intersections
  geom_point(
    data = label_positions,
    aes(
      x = x_position,
      y = y_start
    ),
    color = colors$text,
    size = 1.5
  ) +
  # Add labels
  geom_text(
    data = label_positions,
    aes(
      x = x_position,
      y = y_position,
      label = commodity
    ),
    size = 4.5,
    fontface = "bold",
    color = colors$text,
    vjust = ifelse(label_positions$y_position < 0, 1.2, -0.2)  
  ) +
  # Add trend annotation
  annotate(
    "text", 
    x = 1999, 
    y = -53000, 
    label = str_glue("Overall vegetable production grew\n
                     significantly after 2010,dominated\n
                     by sweet corn and tomatoes with\n
                     over 60K acres each"),
    lineheight = 0.55,
    size = 4,
    fontface = "italic",
    hjust = 0
  ) +
  # Scales
  scale_fill_manual(values = colors$palette) +
  scale_x_continuous(
    breaks = seq(2000, 2025, 5),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1000, suffix = "K"),
    expand = c(0.02, 0),
    position = "right",
    sec.axis = dup_axis(  # Add secondary axis for better title placement
      name = NULL,
      labels = NULL
    )
  ) +
  # Labs
  labs(
    x = "Year",
    y = NULL,
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) +
  # Add custom y-axis title using annotate
  annotate(
    "text",
    x = 1998,  
    y = 0,     
    label = "Acres Harvested\n(Thousands)",
    angle = 90,
    fontface = "bold", 
    size = 3.5,
    vjust = 0.5,
    hjust = 0.5,
    color = "gray30"
  ) + 
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.7),
      family = fonts$title,
      face   = "bold",
      color  = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      family = fonts$subtitle,
      color  = colors$subtitle,
      lineheight = 1.1,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$caption,
      color  = colors$caption,
      lineheight = 1.1,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 15, b = 5)
    ),
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-01
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
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
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggstream    * 0.1.0    2021-05-06 [?] CRAN (R 4.4.1)
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
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────
# > 