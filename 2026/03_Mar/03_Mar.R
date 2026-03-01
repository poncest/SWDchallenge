## Challenge: #SWDchallenge 2026 -- March
## Topic:     Mapping with Purpose — Maternity Care Deserts
## Author:    Steven Ponce
## Date:      2026-03-01
##
## Data:      HRSA Area Health Resources Files (AHRF) 2022-2023
##            https://data.hrsa.gov/data/download
##            Classification logic per March of Dimes (2024):
##            "Nowhere to Go: Maternity Care Deserts Across the US"

### |- Data Source ----
# Primary Source: HRSA Area Health Resources Files (AHRF) 2022-2023
# URL: https://data.hrsa.gov/data/download
# Date Accessed: March 1, 2026
# Files Used: ahrf2023GEO.csv, ahrf2023HP.csv, ahrf2023HF.csv, ahrf2023POP.csv
# Variables: OB-GYN MDs/DOs (2021), APN midwives (NPI, 2022),
#            Hospitals with obstetric care (2021), Birth/postpartum rooms (2021),
#            Live births (July 2021–June 2022)
# Documentation: https://data.hrsa.gov/topics/health-workforce/ahrf

# Classification Reference: March of Dimes (2024)
# Report: "Nowhere to Go: Maternity Care Deserts Across the United States"
# URL: https://www.marchofdimes.org/maternity-care-deserts-report
# Note: Desert % (~40%) differs from MoD (35.1%) due to differences in
#       source coverage (AHRF lacks ABFM and AABC registry data used by MoD)

# County Geometry: U.S. Census Bureau TIGER/Line Shapefiles (2022)
# Accessed via: tigris R package, counties(cb = TRUE, year = 2022)

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  vroom,       # Fast Read of Delimited Files (col_select skips unneeded cols)
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals
  sf,          # Simple Features for R
  tigris       # Load Census TIGER/Line Shapefiles
)

options(tigris_use_cache = TRUE)

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
data_dir <- "2026/03_Mar/"

ahrf_geo <- vroom(
  file.path(data_dir, "ahrf2023GEO.csv"),
  col_select     = c(fips_st_cnty, cnty_name_st_abbrev),
  show_col_types = FALSE
) |> clean_names()

ahrf_hp <- vroom(
  file.path(data_dir, "ahrf2023HP.csv"),
  col_select = c(
    fips_st_cnty, md_nf_obgyn_gen_21, do_nf_obgyn_gen_21,
    apn_midwvs_npi_22
  ),
  show_col_types = FALSE
) |> clean_names()
# NOTE: apn_midwvs_npi_22 = Advanced Practice Nurses / Midwives (NPI registry, 2022)
# More specific than clin_nurse_spec_npi_22 (all clinical nurse specialists).

ahrf_hf <- vroom(
  file.path(data_dir, "ahrf2023HF.csv"),
  col_select = c(
    fips_st_cnty, stgh_obstetrc_care_21,
    stgh_birth_postprtm_rm_21
  ),
  show_col_types = FALSE
) |> clean_names()

ahrf_pop <- vroom(
  file.path(data_dir, "ahrf2023POP.csv"),
  col_select     = c(fips_st_cnty, births_july_1_june_30_22),
  show_col_types = FALSE
) |> clean_names()



## 3. EXAMINE THE DATA ----
# data list
ahrf_list <- list(
  geo = ahrf_geo,
  hp  = ahrf_hp,
  hf  = ahrf_hf,
  pop = ahrf_pop
)

# walk + glimpse
ahrf_list |> walk(glimpse)


## 4. TIDY ----
### |- safe cleaner: replace NA and negative codes with 0 ----
# AHRF sometimes encodes missing as -1, -9, etc.
clean0 <- function(x) replace_na(x, 0) |> pmax(0)

### |- FIPS key ----
ahrf_keys <- ahrf_geo |>
  select(fips = fips_st_cnty, county_name = cnty_name_st_abbrev) |>
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) |>
  filter(!str_ends(fips, "000"))

### |- Clinicians ----
ahrf_clinicians <- ahrf_hp |>
  select(
    fips = fips_st_cnty,
    n_md_obgyn = md_nf_obgyn_gen_21,
    n_do_obgyn = do_nf_obgyn_gen_21,
    n_cnm = apn_midwvs_npi_22
  ) |>
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) |>
  filter(!str_ends(fips, "000"))

### |- Facilities ----
ahrf_facilities <- ahrf_hf |>
  select(
    fips = fips_st_cnty,
    n_ob_hospitals = stgh_obstetrc_care_21,
    n_birth_centers = stgh_birth_postprtm_rm_21
  ) |>
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) |>
  filter(!str_ends(fips, "000"))

### |- Births ----
ahrf_population <- ahrf_pop |>
  select(
    fips = fips_st_cnty,
    n_births = births_july_1_june_30_22
  ) |>
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) |>
  filter(!str_ends(fips, "000"))

### |- Join ----
ahrf_combined <- ahrf_keys |>
  left_join(ahrf_clinicians, by = "fips") |>
  left_join(ahrf_facilities, by = "fips") |>
  left_join(ahrf_population, by = "fips")

### |- Housekeeping ----
rm(
  ahrf_clinicians, ahrf_facilities, ahrf_geo, ahrf_hf,
  ahrf_hp, ahrf_keys, ahrf_pop, ahrf_population
)
gc()

### |- Classify ----
ahrf_classified <- ahrf_combined |>
  mutate(
    # Apply clean0 to all raw counts — handles NA and negative AHRF codes
    across(
      c(n_md_obgyn, n_do_obgyn, n_cnm, n_ob_hospitals, n_birth_centers, n_births),
      clean0
    ),
    n_ob_gyn = n_md_obgyn + n_do_obgyn,
    n_clinicians = n_ob_gyn + n_cnm, # OB-GYN (MD+DO) + APN midwives
    clinician_rate = if_else(n_births > 0, n_clinicians / n_births * 10000, 0),
    n_facilities = n_ob_hospitals + n_birth_centers,
    access_class = case_when(
      n_facilities == 0 & n_clinicians == 0 ~ "Maternity Care Desert",
      n_facilities >= 2 | clinician_rate >= 60 ~ "Full Access",
      TRUE ~ "Limited Access"
    ),
    access_class = factor(
      access_class,
      levels = c("Maternity Care Desert", "Limited Access", "Full Access")
    )
  )

### |- Validation ----
cat("\n--- Classification Distribution ---\n")
ahrf_classified |>
  count(access_class) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()
# Note: ~40% observed vs MoD 35.1% — gap reflects AHRF data scope
# (AHRF lacks ABFM family physician and AABC birth center registries used by MoD)

  
## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    desert  = "#B71C1C",  
    limited = "#D4A574",  
    full    = "#D1D9CE",
    county  = NA,          
    state   = "#FFFFFF"
  )
)

### |- titles and caption ----
title_text <- "Maternity Care Deserts Span Large Regions of the United States"

subtitle_text <- paste0(
  "**4 in 10** U.S. counties have **no obstetric clinicians and no birthing facilities** — ",
  "forming contiguous voids<br>across the Great Plains and rural South."
)

caption_text <- create_swd_caption(
  year        = 2026,
  month       = "Mar",
  source_text = paste0(
    "HRSA Area Health Resources Files (AHRF) 2022–2023<br>",
    "Classification adapted from March of Dimes (2024); estimates may differ due to source coverage"
  )
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background  = element_rect(fill = colors$palette$bg, color = NA),
    panel.background = element_rect(fill = colors$palette$bg, color = NA),
    plot.title = element_text(
      size = 18, face = "bold", color = "#1A1A1A",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_markdown(
      size = 11, color = "#4A4A4A", lineheight = 1.5,
      margin = margin(b = 4)
    ),
    plot.caption = element_markdown(
      size = 7.5, color = "#888888", hjust = 0,
      margin = margin(t = 10)
    ),
    legend.text = element_text(size = 9, color = "#4A4A4A"),
    legend.background = element_rect(fill = NA, color = NA),
    legend.title = element_blank()
  )
)

theme_set(weekly_theme)

### |- county geometry ----
counties_sf <- counties(cb = TRUE, year = 2022, resolution = "20m") |>
  clean_names() |>
  select(fips = geoid, geometry) |>
  filter(!str_sub(fips, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"))

### |- state geometry ----
states_sf <- states(cb = TRUE, year = 2022) |>
  clean_names() |>
  filter(!stusps %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))

### |- join ----
map_df <- counties_sf |>
  left_join(ahrf_classified |> select(fips, access_class), by = "fips")

cat("Unmatched counties:", sum(is.na(map_df$access_class)), "\n")

### |- transform geometry once (faster at high DPI) ----
map_df_t <- st_transform(map_df, 5070)
states_sf_t <- st_transform(states_sf, 5070)

### |- legend labels ----
legend_labels <- c(
  "Maternity Care Desert" = "Maternity Care Desert",
  "Limited Access"        = "Limited Access",
  "Full Access"           = "Full Access"
)

### |- build map ----
ggplot() +
  # Geoms
  geom_sf(
    data      = map_df_t,
    aes(fill  = access_class),
    color     = NA,
    linewidth = 0
  ) +
  geom_sf(
    data      = states_sf_t,
    fill      = NA,
    color     = alpha("#FFFFFF", 0.65),
    linewidth = 0.2
  ) +
  # Scales
  scale_fill_manual(
    values = c(
      "Maternity Care Desert" = colors$palette$desert,
      "Limited Access"  = colors$palette$limited,
      "Full Access" = colors$palette$full
    ),
    breaks   = c("Maternity Care Desert", "Limited Access", "Full Access"),
    labels   = legend_labels,
    na.value = "#E8E8E8",
    drop     = FALSE
  ) +
  coord_sf(datum = NA) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    fill     = NULL
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.5),
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
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(24, "pt"),
    legend.key.height = unit(13, "pt"),
    legend.spacing.x  = unit(8, "pt"),
    legend.margin     = margin(t = 4),
    plot.margin       = margin(t = 16, r = 20, b = 12, l = 20)
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
# date     2026-03-01
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P class         7.3-22   2023-05-03 [?] CRAN (R 4.4.0)
# classInt      0.4-11   2025-01-08 [1] CRAN (R 4.4.3)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P DBI           1.2.2    2024-02-16 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# e1071         1.7-17   2025-12-18 [1] CRAN (R 4.4.3)
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
# P httr          1.4.7    2023-08-15 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# P KernSmooth    2.23-22  2023-07-10 [?] CRAN (R 4.4.0)
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
# proxy         0.4-29   2025-12-29 [1] CRAN (R 4.4.3)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P rappdirs      0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# sf          * 1.1-0    2026-02-24 [1] CRAN (R 4.4.3)
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
# tigris      * 2.2.1    2025-04-16 [1] CRAN (R 4.4.3)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# units         1.0-0    2025-10-09 [1] CRAN (R 4.4.3)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P uuid          1.2-0    2024-01-14 [?] CRAN (R 4.4.0)
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom       * 1.7.0    2026-01-27 [?] CRAN (R 4.4.3)
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
