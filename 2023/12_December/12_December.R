

# Challenge:  #SWDchallange 2023 -- December
## Topic:     visualize holiday traditions
## Data:      synthetic data
## Author:    Steven Ponce
## Date:      2023-12-06



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext)
pacman::p_load(camcorder, scales)



# |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 6,
  units  = "in",
  dpi    = 320) 


# |- resolution ---- 
showtext_opts(dpi = 320)



## 2. GENERATE IN THE DATA ----

set.seed(4321)

# Create tibbles for each group
tibble_01 <- tibble(x = runif(900),   y = runif(900),   color = c("#35aedd"), size = 2.5, alpha = 0.85, id = "A")
tibble_02 <- tibble(x = runif(1400),  y = runif(1400),  color = c("#94d3e6"), size = 2.5, alpha = 0.85, id = "B")
tibble_03 <- tibble(x = runif(900),   y = runif(900),   color = c("#2070b9"), size = 2.5, alpha = 0.85, id = "C")
tibble_04 <- tibble(x = runif(2500),  y = runif(2500),  color = c("#8cb4cc"), size = 2.5, alpha = 0.85, id = "D")
tibble_05 <- tibble(x = runif(13000), y = runif(13000), color = c("#d6e4eb"), size = 2.5, alpha = 0.5, id = "E")


# Combine all the tibbles into one
data_df <- bind_rows(tibble_01, tibble_02, tibble_03, tibble_04, tibble_05)



# 3. VISUALIZATION ---- 

# |- plot aesthetics ---- 
plot_col     <- "#fffafa"   
panel_col    <- "#526D86" 
title_col    <- 'black'
caption_col  <- "black"


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: December 2023 &bull; Source: none<br>")
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>") 
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Snowflakes")

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Raleway Dots", family = "title")                        
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



# |-  final plot ----
data_df %>% 
  ggplot(aes(x = x, y = y, 
         color = color, alpha = alpha, group = id)) +
  
  # geoms
  geom_point() +
  
  # scales
  scale_color_identity() +
  scale_alpha_identity() +
  
  # labs
  labs(
    title    = title_text,
    caption  = caption_text
    ) +
  
  # theme
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "plot",
    
    plot.background       = element_rect(fill = plot_col, color = plot_col),
    panel.background      = element_rect(fill = panel_col, color = panel_col),
    
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    
    plot.title            = element_text(
      family              = 'title',
      color               = title_col,
      face                = "bold",
      size                = 50, 
      margin              = margin(t = 10, b = 5)),
    
    plot.caption          = element_markdown(
      family              = 'caption',
      color               = caption_col,
      lineheight          = 0.6,
      size                = 10,
      hjust               = 0.5,
      halign              = 0.5,
      margin              = margin(t = 10, b = 10)),
    )




# 4. SESSION INFO ---- 

sessioninfo::session_info(include_base = TRUE) 
# ─ Session info ──────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-12-06
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────
# package     * version  date (UTC) lib source
# base        * 4.3.1    2023-06-16 [2] local
# camcorder   * 0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cli           3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark    1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler      4.3.1    2023-06-16 [2] local
# curl          5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets    * 4.3.1    2023-06-16 [2] local
# dplyr       * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi         1.0.5    2023-10-08 [1] CRAN (R 4.3.2)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2     * 3.4.4    2023-10-12 [1] CRAN (R 4.3.2)
# ggtext      * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski        1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue          1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics    * 4.3.1    2023-06-16 [2] local
# grDevices   * 4.3.1    2023-06-16 [2] local
# grid          4.3.1    2023-06-16 [2] local
# gridtext      0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable        0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here          1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms           1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# jsonlite      1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle     1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate   * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick        2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr      2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown      1.11     2023-10-19 [1] CRAN (R 4.3.2)
# methods     * 4.3.1    2023-06-16 [2] local
# munsell       0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman        0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# pillar        1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr       * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6            2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg          1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp          1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr       * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# rlang         1.1.2    2023-11-04 [1] CRAN (R 4.3.2)
# rprojroot     2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi    0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg          2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
# sessioninfo   1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext    * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb  * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# stats       * 4.3.1    2023-06-16 [2] local
# stringi       1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr     * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite       2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts    * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts   1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textshaping   0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble      * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr       * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect    1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidyverse   * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange    0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools         4.3.1    2023-06-16 [2] local
# tzdb          0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# utf8          1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# utils       * 4.3.1    2023-06-16 [2] local
# vctrs         0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# withr         2.5.2    2023-10-30 [1] CRAN (R 4.3.2)
# xfun          0.41     2023-11-01 [1] CRAN (R 4.3.2)
# xml2          1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────
# > 

