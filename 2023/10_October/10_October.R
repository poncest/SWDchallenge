

# Challenge:  #SWDchallange 2023 -- October
## Topic:     annotate it!
## Data:      {babynames} R package
## Author:    Steven Ponce
## Date:      2023-10-02


## 0. DATA SOURCE ----
#' US baby names from the SSA via R {baynames} package
#' https://github.com/hadley/babynames

#' Reference: Leo Messi Timeline
#' https://www.messifanpage.com/timeline/



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, janitor, scales, lubridate)
pacman::p_load(babynames)
theme_set(theme_light(base_size = 16))


# |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320)


## 2. READ IN THE DATA ----
babynames <- babynames::babynames



## 3. EXAMINING THE DATA ----
glimpse(babynames) 

babynames$name %>% unique()



## 4. TIDYDATA ---- 

# clean data
# selected_name <- c("Alexa", "Echo", "Siri", "Cortana")
selected_name <- c("Leo")


data_tbl <- babynames %>%
  # filter for selected names
  filter(name %in% selected_name) %>% 
  
  # add decade column
  mutate(decade = (year - 1)%/% 10 * 10) %>% 
 
  # summary
  group_by(name, year) %>% 
  summarise(total_prop = sum(prop)) %>%
  ungroup() %>% 
  
  # filter year
  filter(year > 1900)



# 5. VISUALIZATION ---- 

# |- plot aesthetics ---- 
bkg_col      <- "#072e51"
title_col    <- "#f2f2f2"                
subtitle_col <- "#f2f2f2"     
caption_col  <- "#f2f2f2"     
text_col     <- "#f2f2f2"     
ann_col      <- "#ad8408"
col_palette <- c("Leo" = "#addcff")


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: October 2023 &bull; Source: R babynames package<br>")
X <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>") 
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text    <- str_glue("The Popularity of Leo, 1990-2017")

subtitle_text <- str_glue("It's interesting to note that <span style='font-size:16pt; color:#addcff'>**Leo**</span> is still a popular choice for parents when it comes to naming<br>",
                          "their babies in the USA. The Social Security Administration confirms that Leo is among<br>",
                          "the top names for boys, and it's not hard to see why. <span style='font-size:16pt; color:#addcff'>**Leo Messi**</span>, the soccer superstar,<br>",
                          "continues toimpress with his exceptional skills on the field. It's no wonder that parents<br>",
                          "are inspired by his performance of dreams and choose to name their babies after him.<br>",
                          )


caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Chakra Petch", family = "title")                            
font_add_google("Chakra Petch", family = "subtitle")              
font_add_google("Chakra Petch", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



### |- Main Plot -----
p1 <- data_tbl %>% 
  ggplot(aes(x = year, y = total_prop, color = name))+
  geom_line(linewidth = 1)+

  # scales
  scale_x_continuous(breaks = pretty_breaks(n = 10))+
  scale_y_continuous(labels = percent_format())+
  scale_color_manual(values = col_palette)+
  
  # labs
  labs(x = "Year",
       y = "Percentage of Total Babies Named",
       title    = title_text,
       subtitle = subtitle_text,
       caption  = caption_text) +
  
  # theme
  theme_minimal(base_family = 'text')+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.margin           = margin(t = 10, r = 15, b = 10, l = 15),
    
    axis.title.x          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(t = 12)), 
    axis.title.y          = element_text(size = 14, face = 'bold', color = text_col, margin = margin(r = 12)), 
    
    axis.text             = element_text(size = 12, color = text_col),
    axis.line.x           = element_line(color = text_col),
    
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.25, color = 'gray'),
    panel.grid.minor.y    = element_blank(),
    
    panel.grid.major.x    = element_blank(),
    panel.grid.minor.x    = element_blank(),
    
    plot.title            = element_text(
      family              = 'title',
      color               = title_col,
      face                = "bold",
      size                = 30,  
      margin              = margin(t = 10, b = 5)),
    
    plot.subtitle         = element_markdown(
      family              = 'subtitle',
      color               = title_col,
      lineheight          = 0.8, 
      size                = 15,  
      margin              = margin(t = 5, b = 10)),
    
    plot.caption          = element_markdown(
      family              = 'caption',
      color               = caption_col,
      lineheight          = 0.6,
      size                = 10,
      hjust               = 0.5,
      halign              = 0.5,
      margin              = margin(t = 10, b = 10)),
  )

p1



### |-   DF for annotations -----

# Simple Leo Messi timeline

born_df           <- tribble(~year, ~total_prop, ~label_text,
                             1987,   0.001,      "Lionel Messi is Born\n24 June 1987")

trial_contract_df  <- tribble(~year, ~total_prop, ~label_text,
                             2000,   0.002,       "Trial with Barcelona\nFirst Contract\n14 December 2000")

prof_contract_df  <- tribble(~year, ~total_prop, ~label_text,
                             2004,   0.003,       "First Professional Contract\n4 February 2004")

goal_arg_df       <- tribble(~year, ~total_prop, ~label_text,
                             2005,   0.004,       "First Goal with Argentina\n1 March 2006")

gold_df           <- tribble(~year, ~total_prop, ~label_text,
                             2008,   0.005,       "Gold Medal at Olympic Games\n23 August 2008")

ballon_4_df       <- tribble(~year, ~total_prop, ~label_text,
                             2013,   0.005,       "4 Ballon D'or in a Row\n7 January 2013")

goal_500_df       <- tribble(~year, ~total_prop, ~label_text,
                             2017,   0.005,       "500 goals with Barcelona\n23 April 2017")


#' Now the NOT so fun part... annotations

### |-  Annotated Plot -----
p1 +

# 1.0 Lionel Messi is Born
geom_text(data = born_df,
        aes(x = 1953, y = 0.0015, label = label_text),
        family     = 'text',
        lineheight = 0.9,
        hjust      = 0.35,
        size       = 4,
        color      = ann_col) + 

geom_point(data = born_df,
         aes(x = year, y = 0.00023), color = ann_col, size = 4, shape = 21, stroke = 1.5) +

annotate("curve", x = 1966, xend = 1986, y = 0.0014, yend = 0.0004,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.3, color = ann_col, linewidth = 0.2)  +     


# 2.0 The Trial with Barcelona & First Contract
geom_text(data = trial_contract_df,
          aes(x = 1955, y = 0.0021, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) +
  
geom_point(data = trial_contract_df, 
          aes(x = year, y = 0.00032), color = ann_col, size = 3, shape = 21, stroke = 1.5) +
  
annotate("curve", x = 1973, xend = 1999, y = 0.0019, yend = 0.0004,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.3, color = ann_col, linewidth = 0.2) +
  
# 3.0 First Professional Contract
geom_text(data = prof_contract_df,
          aes(x = 1956, y = 0.0026, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) + 

geom_point(data = prof_contract_df, 
           aes(x = year-1, y = 0.00053), color = ann_col, size = 3, shape = 21, stroke = 1.5)+

annotate("curve", x = 1973, xend = 2002, y = 0.0025, yend = 0.0006,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.3, color = ann_col, linewidth = 0.2) +

# 4.0 First Goal with Argentina
geom_text(data = goal_arg_df,
          aes(x = 1959, y = 0.0031, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) +

geom_point(data = goal_arg_df,
           aes(x = year+1, y = 0.00069), color = ann_col, size = 3, shape = 21, stroke = 1.5)+
  
annotate("curve", x = 1973, xend = 2005, y = 0.0030, yend = 0.0008,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.3, color = ann_col, linewidth = 0.2) +
  
# 5.0 Gold Medal at Olympic Games
geom_text(data = gold_df,
          aes(x = 1966, y = 0.0035, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) +

geom_point(data = gold_df,
           aes(x = year, y = 0.00073), color = ann_col, size = 3, shape = 21, stroke = 1.5)+

annotate("curve", x = 1982, xend = 2007.5, y = 0.0034, yend = 0.00089,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.3, color = ann_col, linewidth = 0.2) +
  
# 6.0 4 Ballon D'or in a Row
geom_text(data = ballon_4_df,
          aes(x = 1980, y = 0.0039, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) +
  
geom_point(data = ballon_4_df,
           aes(x = year, y = 0.0018), color = ann_col, size = 3, shape = 21, stroke = 1.5)+
  
annotate("curve", x = 1996, xend = 2012, y = 0.0038, yend = 0.0019,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.15, color = ann_col, linewidth = 0.2) +


# 7.0 500 goals with Barcelona
geom_text(data = goal_500_df,
          aes(x = 1988, y = 0.0043, label = label_text),
          family     = 'text',
          lineheight = 0.9,
          hjust      = 0.35,
          size       = 4,
          color      = ann_col) +

geom_point(data = goal_500_df,
           aes(x = year, y = 0.0029), color = ann_col, size = 3, shape = 21, stroke = 1.5)+

annotate("curve", x = 2000, xend = 2015.5, y = 0.0042, yend = 0.0030,
         arrow = arrow(length = unit(0.02, "npc"), type = "open"),
         curvature = -0.15, color = ann_col, linewidth = 0.2) 




sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-10-03
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# babynames    * 1.0.1    2021-04-12 [1] CRAN (R 4.3.1)
# base         * 4.3.1    2023-06-16 [2] local
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1    2023-06-16 [2] local
# curl           5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets     * 4.3.1    2023-06-16 [2] local
# dplyr        * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi          1.0.4    2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here           1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# scales       * 1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.4    2022-02-11 [1] CRAN (R 4.3.0)
# textshaping    0.3.6    2021-10-13 [1] CRAN (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.0    2023-01-24 [1] CRAN (R 4.3.0)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.1    2023-06-16 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# withr          2.5.0    2022-03-03 [1] CRAN (R 4.3.0)
# xfun           0.40     2023-08-09 [1] CRAN (R 4.3.1)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ────────────────────────────────────────────────────────────────────────────────────────────
# > 
