

# Challenge:  #SWDchallenge 2024 -- January
## Topic:     upskill with UpSets
## Data:      Patient Risk Profiles
## Author:    Steven Ponce
## Date:      2024-01-xx



## 0. DATA SOURCE ----
#' 
#' Patient Risk Profiles
#' TidyTuesday 2023 - Week 43 via Jenna Reps
#' 
#' This dataset contains 100 simulated patient's medical history features and the predicted 1-year 
#' risk of 14 outcomes based on each patient's medical history features. The predictions used 
#' real logistic regression models developed on a large real world healthcare dataset.
#' 
#' Reference: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-10-24/readme.md



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, camcorder, scales, janitor)
pacman::p_load(UpSetR, grid, gridExtra)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 43) 

patient_risk_profiles <- tt$patient_risk_profiles |>  clean_names()  |>  glimpse()

rm(tt)


## 3. EXAMINE THE DATA ----
glimpse(patient_risk_profiles)
colnames(patient_risk_profiles)


## 4. TIDY ----
patient_risk_profiles <- as.data.frame(patient_risk_profiles) |> 
  select(-c(person_id, sex_male, sex_female))


# 5. VISUALIZATION ---- 

### |-  main plot ----

### |-  titles and caption ----

title_text    <- str_glue("\n100 simulated patient's medical history features and the predicted 1-year risk\nof 14 outcomes based on each patient's medical history features.") 

caption_text  <- str_glue("#SWDchallenge: Jan 2024 . Source: TidyTuesday 2023 wk 43\nVisualization: @sponce1 . Tools: #rstats #ggplot2\n\n")

# Draw Upset Plot
main <- upset(data = patient_risk_profiles,  
              nsets               = 5, 
              nintersects         = NA,  # plot all intersects
              matrix.color        = "gray23",
              main.bar.color      = "gray23",
              mainbar.y.label     = "Intersection Size", 
              mainbar.y.max       = NULL,
              sets.bar.color      = "gray23", 
              sets.x.label        = "Set Size",
              point.size          = 4,         #2.2, 
              line.size           = 0.7, 
              mb.ratio            = c(0.7, 0.3),
              att.pos             = "top", 
              order.by            = "freq", 
              decreasing          = TRUE,
              show.numbers        = "yes", 
              shade.color         = "gray88",    # change
              shade.alpha         = 0.25, 
              matrix.dot.alpha    = 0.5,
              color.pal           = 2, 
              scale.intersections = "identity",
              scale.sets          = "identity", 
              text.scale          = c(1.6, 1.6, 1.6, 1.5, 1.5, 1.5), 
              set_size.show       = TRUE, 
              query.legend        = "none",
              queries = list(list(query = elements,
                                  params = list("inflammatory_bowel_disease_in_prior_year"),
                                  active = T))
) 

main



# Edit the Description of a Grid Graphical Object
grid::grid.edit('arrange', name = "patient_risk_profiles")

# Grab the current grid output; vp = viewport
vp <- grid::grid.grab()

# Arrange multiple grobs on a page
my_grid <- gridExtra::grid.arrange(
  grobs = list(
    vp
  ),
  top    = textGrob(title_text, gp = gpar(fontsize = 14, font = 2)),
  bottom = textGrob(caption_text, gp = gpar(fontsize = 9,font = 1)),
  left   = "",
  right  = "",
  cols   = 1,
  nrow   = 1,
) 


# Save Image
ggsave(filename = "01_Jan.png", 
       path   = '2024/01_Jan/img/',
       plot   = my_grid,
       dpi    = 320,
       bg     = 'white',
       width  = 10, 
       height = 8, 
       units  = "in",
       )



## 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ─────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-01-09
# rstudio  2023.12.0+369 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.2    2023-10-31 [2] local
# bit            4.0.5    2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5    2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# compiler       4.3.2    2023-10-31 [2] local
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.2.0    2023-12-08 [1] CRAN (R 4.3.2)
# datasets     * 4.3.2    2023-10-31 [2] local
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.3.2)
# fansi          1.0.6    2023-12-08 [1] CRAN (R 4.3.2)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.4    2023-10-12 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.2    2023-10-31 [2] local
# grDevices    * 4.3.2    2023-10-31 [2] local
# grid         * 4.3.2    2023-10-31 [2] local
# gridExtra    * 2.3      2017-09-09 [1] CRAN (R 4.3.0)
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.8    2023-12-04 [1] CRAN (R 4.3.2)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3    2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.8.2    2023-12-20 [1] CRAN (R 4.3.2)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# methods      * 4.3.2    2023-10-31 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.2    2023-10-31 [2] local
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# plyr           1.8.9    2023-10-02 [1] CRAN (R 4.3.2)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.7    2023-12-11 [1] CRAN (R 4.3.2)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# rlang          1.1.2    2023-11-04 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.6.0    2023-10-08 [1] CRAN (R 4.3.2)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.2    2023-10-31 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.2    2023-10-31 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# UpSetR       * 1.4.0    2019-05-22 [1] CRAN (R 4.3.2)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# utils        * 4.3.2    2023-10-31 [2] local
# vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.2    2023-10-30 [1] CRAN (R 4.3.2)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.2/library
# 
# ────────────────────────────────────────────────────────────────────────────
# > 
