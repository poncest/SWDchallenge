

# Challenge:  #SWDchallange 2023 -- September
## Data:      makeover magic
## Author:    Steven Ponce
## Date:      2023-09-02


## 0. DATA SOURCE ----
#' Original data and graph can be found at:
#' https://docs.google.com/spreadsheets/d/1yduB4BsbwzpSUk1R6OImlo_CHrILAhWM/edit#gid=580597745
#' 


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, scales, lubridate)
pacman::p_load(geomtextpath)
theme_set(theme_light())


# |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320)


## 2. READ IN THE DATA ----
raw_data <- read_csv("2023/09_September/data/SEP 2023 SWDchallenge.csv") %>% 
  clean_names()



## 3. EXAMINING THE DATA ----
glimpse(raw_data) 
colnames(raw_data)

raw_data$date %>% unique()



## 4. TIDYDATA ---- 

# clean data
clean_data <- raw_data %>%
  # fix date
  mutate(date  = ym(date),
         month = month(date, label = TRUE, abbr = TRUE),
         year  = year(date)) %>% 
  
  arrange(date) %>% 
  
  # projected?
  mutate(status = ifelse(date > "2023-09-01", "projected", "actual")) %>% 
  
  # add month-to-moth growth & pct_goal
  mutate(
    mom_growth = (number_accounts - lag(number_accounts))/lag(number_accounts) * 100,
    pct_goal   = number_accounts / last(number_accounts)
    ) %>% 
  
  # position of labels
  mutate(label_position = ifelse(mom_growth > 0, 5.5, -8)) %>%                 
  
  # bar labels
  mutate(bar_label = str_glue("{scales::number(mom_growth, accuracy = 0.01)} %"))


# actual data
actual_tbl <- clean_data %>% 
  filter(status == "actual")
  
# projected data
projected_tbl <- clean_data %>% 
  filter(status == "projected")



# 5. VISUALIZATION ---- 

# |- plot aesthetics ---- 
bkg_col      <- "#000000"
title_col    <- "#fafafa"                 
subtitle_col <- "#fafafa" 
caption_col  <- "#fafafa" 
text_col     <- "#fafafa"   

col_palette <- c("actual" = "#8a74b6", "projected" = "#b9b7e0")


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: September 2023 &bull; Source: Storytelling with Data: Let's Practice<br>")
X <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>") 
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text <- str_glue("Slow and Steady Growth")

subtitle_text <- str_glue("We have made significant progress in the last nine months, achieving **77%** of our goal of 450 accounts. Our accounts-to-managers<br>",
                          "ratio is impressively low at just 16, which has helped us to optimize our resources and provide excellent service to our clients.<br>",
                          "Our projections indicate that we can continue with slow and steady growth, ultimately reaching our target within 24 months with<br>",
                          " a slightly higher ratio of 25<br>")

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Shippori Antique B1", family = "title")                            
font_add_google("Source Sans Pro", family = "subtitle")              
font_add_google("Yaldevi", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



### |- Main Plot -----
p1 <- clean_data %>% 
  ggplot(aes(date, pct_goal, color = status))+
  
  # geoms
  # actual
  geom_textline(data = actual_tbl, 
                label = "ACTUAL", size = 6, vjust = -0.5,
                linewidth = 0.8, linecolor = col_palette[1], linetype = 1, 
                color = col_palette[1]) +
  
  geom_point(data  = actual_tbl %>% filter(date == "2023-09-01"),
             shape = 19,
             size  = 4)+

  # projected
  geom_textline(data = projected_tbl, 
                label = "PROJECTED", size = 6, vjust = -0.5,
                linewidth = 0.8, linecolor = col_palette[2], linetype = 2, 
                color = col_palette[2]) +
  
  geom_point(data  = projected_tbl %>% filter(date == "2024-12-01"),
             shape = 19,
             size  = 4)+
  
  # scales
  scale_x_date(breaks = "3 month",
               date_minor_breaks = "1 month",
               labels = label_date_short())+
  
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  
  scale_color_manual(values = col_palette) +
  
  coord_cartesian(clip = "off")+
  
  # labs
  labs(x = "Date",
       y = "Percent<br>to Goal",
       title    = title_text,
       subtitle = subtitle_text,
       caption  = caption_text) +
  
  # theme
  theme_minimal(14, base_family = 'text')+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background     = element_rect(fill = bkg_col, color = bkg_col),
    panel.background    = element_rect(fill = bkg_col, color = bkg_col),
     
    axis.text           = element_text(color = text_col),
     
    axis.title.y        = element_markdown(color = text_col, face = 'bold', size = 12,
                                           margin = margin (r = 12), hjust = 0.5,
                                           vjust = 0.96, angle = 0),
     
    axis.title.x        = element_text(color = text_col, face = 'bold', size = 12,
                                       margin = margin(t = 10), hjust = 0.5),
     
    axis.line.x         = element_line(color = text_col, linewidth = 0.2),
    axis.ticks.x        = element_line(color = text_col, linewidth = 0.2),
    
    panel.grid.major.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
     
    panel.grid.minor    = element_blank(),
    panel.grid.major    = element_blank(),
     
    plot.margin         = margin(t = 20, r = 20, b = 20, l = 20),
     
    plot.title          = element_markdown(
      family            = 'title',
      color             = title_col,
      lineheight        = 1.0, 
      face              = "bold",
      size              = 18,  
      margin            = margin(t = 5, b = 5)),
     
    plot.subtitle       = element_markdown(
      family            = 'subtitle',
      color             = title_col,
      lineheight        = 1.0, 
      size              = 10,
      margin            = margin(b = 10)),
    
    plot.caption        = element_markdown(
      family            = 'caption',
      color             = caption_col,
      lineheight        = 0.6,
      size              = 11,
      hjust             = 1,
      halign            = 1,
      margin            = margin(t = 10, b = 5)),
  )  

p1



### |-  Annotated Plot -----
# DF for annotations
actual_df     <- tibble(date = "2023-09-01", label_text = "77%\nRatio = 16")
projected_df   <- tibble(date = "2024-12-01", label_text = "100%\nRatio = 25")


p1+
  # actual
  geom_text(data = actual_df, 
            aes(x = as.Date(date), y = 0.65, label = label_text),
            family     = 'text',
            #fontface   = "bold",
            lineheight = 0.9,
            hjust      = 0.35,
            size       = 5,
            color      = col_palette[1]) +
  
  # projected
  geom_text(data = projected_df, 
            aes(x = as.Date(date), y = 0.9, label = label_text),
            family     = 'text',
            #fontface   = "bold",
            lineheight = 0.9,
            hjust      = 0.55,
            size       = 5,
            color      = col_palette[2])


sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-09-03
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder      0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0   2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1   2023-06-16 [2] local
# crayon         1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# datasets     * 4.3.1   2023-06-16 [2] local
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
# geomtextpath * 0.1.1   2022-08-30 [1] CRAN (R 4.3.1)
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0  2023-05-19 [1] CRAN (R 4.3.0)
# glue           1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1   2023-06-16 [2] local
# grDevices    * 4.3.1   2023-06-16 [2] local
# grid           4.3.1   2023-06-16 [2] local
# gridtext       0.1.5   2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.3   2023-03-21 [1] CRAN (R 4.3.0)
# here           1.0.1   2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.6   2023-05-08 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0   2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.5   2023-06-05 [1] CRAN (R 4.3.0)
# labeling       0.4.2   2020-10-20 [1] CRAN (R 4.3.0)
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.7     2023-05-16 [1] CRAN (R 4.3.0)
# methods      * 4.3.1   2023-06-16 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1   2023-06-16 [2] local
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5   2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.10  2023-01-22 [1] CRAN (R 4.3.0)
# readr        * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.2   2023-02-09 [1] CRAN (R 4.3.0)
# rlang          1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3   2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.14    2022-08-22 [1] CRAN (R 4.3.0)
# rsvg           2.4.0   2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3   2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6   2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.0  2019-05-25 [1] CRAN (R 4.3.0)
# stats        * 4.3.1   2023-06-16 [2] local
# stringi        1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0   2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1   2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8   2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6   2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1   2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0   2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR * 1.0.2   2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.1   2023-06-16 [2] local
# tzdb           0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.0   2023-06-06 [1] CRAN (R 4.3.0)
# utf8           1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1   2023-06-16 [2] local
# vctrs          0.6.2   2023-04-19 [1] CRAN (R 4.3.0)
# vroom          1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.39    2023-04-20 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ───────────────────────────────────────────────────────────────────────────────────────────
# > 
