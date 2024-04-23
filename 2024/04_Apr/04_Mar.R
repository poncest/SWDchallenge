
# Challenge:  #SWDchallenge 2024 -- Apr
## Topic:     APR 2024 | Napoleon's April
## Data:      Minard's Map about Napoleon's 1812 March
## Author:    Steven Ponce
## Date:      2024-04-23


## 0. DATA SOURCE ----

#' Data:Napoleon's 1812 March
#' Link: https://docs.google.com/spreadsheets/d/1FzFYcSjOgMRf5Hxvzy9Ck9Zr63ikZIFz/edit?usp=drive_link&ouid=109391454461841910953&rtpof=true&sd=true


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  camcorder,   # Record Your Plot History
  scales,      # Scale Functions for Visualization
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  MoMAColors,  # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  readxl       # Read Excel Files
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 5,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
troops <- read_xlsx(path = "2024/04_Apr/APR24 challenge - Napoleon.xlsx", 
          sheet = "troops", 
          col_names = TRUE, 
          trim_ws   = TRUE,
          .name_repair = "unique") |> 
  clean_names() 


## 3. EXAMINE THE DATA ----
glimpse(troops)
colnames(troops)
skimr::skim(troops)


## 4. TIDY ----


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#fafafa', 0.5)    
title_col    <- "#414040"            
subtitle_col <- "gray40"     
caption_col  <- "gray30"   
text_col     <- colorspace::darken("#83796f" , 0.15)    
col_palette  <- MoMAColors::moma.colors("Alkalay1", n = 5, type = "discrete", direction = -1)[c(2,4)]
 
### |-  titles and caption ----
tt <- str_glue("#SWDchallenge: April 2024 &bull; Source: Provided by STWD<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

advancing     <- str_glue("<span style='color:{ col_palette[1] }'>**Advancing**</span>")
retreating    <- str_glue("<span style='color:{ col_palette[2] }'>**Retreating**</span>")

title_text    <- str_glue("Troop Losses During Napoleon's 1812 Russian Campaign")

subtitle_text <- str_glue("An analysis of troop losses over time, showing the significant decline in<br>
                          survivor numbers during { advancing } and { retreating } phases.")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("PT Sans", regular.wt = 400, family = "title")                 
font_add_google("Noto Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Noto Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "plot",
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
  axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "#252525", linewidth = .2),
  panel.grid.major.y    = element_line(linetype = "solid", linewidth = 0.1, color = "#D3D3D3"),
  panel.grid.minor.y    = element_blank(),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)

### |-  final plot ----
troops |> 
  
  ggplot(aes(x = seq_along(survivors), y = survivors, color = direction)) +
  
  # Geoms
  geom_line(linewidth = 1) +
  
  # Scales
  scale_x_continuous() +
  scale_y_continuous(
    breaks = seq(0, 350000, by = 50000),
    limits = c(0, 350000),
    labels = number_format(scale = 1/1e3, suffix = " K")
  ) +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off") +
  
  # Labs
  labs(
    x = "Point in Time",
    y = "Number of Survivors",
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
    ) +
  
  # Theme
  theme(
    plot.title      = element_markdown(
      size          = rel(1.25),
      family        = "title",
      face          = "bold",
      color         = title_col,
      lineheight    = 1.1,
      margin        = margin(t = 5, b = 5)
    ),
    plot.subtitle   = element_markdown(
      size          = rel(.9),
      family        = "subtitle",
      # face          = "bold",
      color         = subtitle_col,
      lineheight    = 1.1,
      margin        = margin(t = 0, b = 5)
    ),
    plot.caption    = element_markdown(
      size          = rel(.65),
      family        = "caption",
      color         = caption_col,
      lineheight    = 0.65,
      hjust         = 0.5,
      halign        = 0.5,
      margin        = margin(t = 5, b = 5)
    ),
  )


## 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.3 (2024-02-29 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-04-23
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.3    2024-02-29 [2] local
# base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.3    2024-02-29 [2] local
# curl           5.2.0    2023-12-08 [1] CRAN (R 4.3.2)
# datasets     * 4.3.3    2024-02-29 [2] local
# digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.1)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.3.2)
# fansi          1.0.6    2023-12-08 [1] CRAN (R 4.3.2)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.5.0    2024-02-23 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.7.0    2024-01-09 [1] CRAN (R 4.3.2)
# graphics     * 4.3.3    2024-02-29 [2] local
# grDevices    * 4.3.3    2024-02-29 [2] local
# grid           4.3.3    2024-02-29 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# htmltools      0.5.7    2023-11-03 [1] CRAN (R 4.3.2)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.8    2023-12-04 [1] CRAN (R 4.3.2)
# knitr          1.45     2023-10-30 [1] CRAN (R 4.3.2)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3    2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.12     2023-12-06 [1] CRAN (R 4.3.2)
# MetBrewer      0.2.0    2022-03-21 [1] CRAN (R 4.3.1)
# methods      * 4.3.3    2024-02-29 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.7    2023-12-11 [1] CRAN (R 4.3.2)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.5    2024-01-10 [1] CRAN (R 4.3.2)
# readxl       * 1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# repr           1.1.6    2023-01-26 [1] CRAN (R 4.3.1)
# rlang          1.1.3    2024-01-10 [1] CRAN (R 4.3.2)
# rprojroot      2.0.4    2023-11-05 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.6.0    2023-10-08 [1] CRAN (R 4.3.2)
# rvest          1.0.4    2024-02-12 [1] CRAN (R 4.3.2)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# skimr          2.1.5    2022-12-23 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.3    2024-02-29 [2] local
# stringi        1.8.3    2023-12-11 [1] RSPM (R 4.3.0)
# stringr      * 1.5.1    2023-11-14 [1] RSPM (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.0.5    2023-10-09 [1] RSPM (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.3.2)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.3    2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.3    2024-02-29 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.3    2024-02-19 [1] CRAN (R 4.3.3)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# utils        * 4.3.3    2024-02-29 [2] local
# vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# withr          3.0.0    2024-01-16 [1] CRAN (R 4.3.2)
# xfun           0.41     2023-11-01 [1] CRAN (R 4.3.2)
# xml2           1.3.6    2023-12-04 [1] RSPM (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.3/library
# 
# ───────────────────────────────────────────────────────────────────────────────────────────
# > 