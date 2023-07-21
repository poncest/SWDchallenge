

# Challenge:  #SWDchallange 2023 -- July
## Data:      bring on the bubbles
## Author:    Steven Ponce
## Date:      2023-07-12


## 0. DATA SOURCE ----
#' TidyTuesday 2023 - Week 03
#' Art History
#' Reference: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-01-17/readme.md


## 0. LINKS ----
#' challenge info: https://community.storytellingwithdata.com/challenges/jul-2023-bring-on-the-bubbles
#' my contribution: https://community.storytellingwithdata.com/challenges/jul-2023-bring-on-the-bubbles/art-history-books



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)


# |- figure size ---- 
gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 6,
  units  = "in",
  dpi    = 600) 

# |- resolution ---- 
showtext_opts(dpi = 600)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2023, week = 03) 
artists <- tt$artists %>% clean_names()

readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
glimpse(artists) 
colnames(artists)

artists$artist_ethnicity %>% unique()
artists$artist_nationality %>% unique()


## 4. TIDYDATA ---- 
artist_tbl <- artists %>%
  rename_at(vars(matches("artist_")), ~ str_remove(., "artist_")) %>% 
  filter(gender != "N/A") %>% 

  group_by(name, nationality, book, gender) %>% 
  summarise(
    count = n(), 
    space_ratio_per_page = sum(space_ratio_per_page_total),
    avg_space_ratio_per_page = sum(space_ratio_per_page_total) / count,
  ) %>% 
  ungroup() %>% 
  arrange(desc(count)) 


# 5. VISUALIZATION ---- 

# |- plot aesthetics ---- 
bkg_col      <- "#323238"
title_col    <- "#fafafa"                 
subtitle_col <- "#fafafa" 
caption_col  <- "#706f6f"
text_col     <- "#fafafa" 
col_palette  <- c("Female" = "#ec93bd", "Male" = "#5ec1cb")


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: July 2023 &bull; Source: arthistory data package<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text <- str_glue("Art History Books")

female <- str_glue("<span style='font-size:12pt; color: #ec93bd'>**female**</span>")
male   <- str_glue("<span style='font-size:12pt; color: #5ec1cb'>**male**</span>")

subtitle_text <- str_glue("It is worrisome to note that only 13% of the artists featured in Gardner's or Janson's textbook are<br>",
                          "{female}, which amounts to just 54 artists. On the other hand, {male} artists dominate the textbook<br>",
                          "with 87% appearances, totaling 484 artists.")

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Audiowide", family = "title")                            
font_add_google("Ruda", family = "subtitle")              
font_add_google("Ruda", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  


# |-  main plot ----
p1 <- artist_tbl %>% 
  ggplot(aes(x = fct_reorder(book,avg_space_ratio_per_page), y = avg_space_ratio_per_page, fill = gender)) +
  
  # geometries
  geom_point(aes(size = count), shape = 21, alpha = 0.95, na.rm = TRUE,
             position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1, seed = 123)) +
  
  # scales
  scale_x_discrete()+
  
  scale_y_continuous(breaks = seq(0, 4, by = 1),
                     limits = c(0, 4)) +
  
  scale_size_continuous(range = c(1,5), name = "")+ 
  
  scale_fill_manual(values = col_palette) +
  
  coord_flip(clip = 'off') + 
  
  # labs
  labs(x = "", y = "Page Coverage (cm2)",
       title    = title_text,
       subtitle = subtitle_text,
       caption  = caption_text
  ) +
  
  # theme
  theme_minimal(
    base_size   = 12,
    base_family = 'text'
  ) +
  
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',

    
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.text          = element_text(color = text_col),
    axis.title         = element_text(color = text_col, face = 'bold', size = 12),

    axis.line.x        = element_line(color = text_col, linewidth = 0.2),
    axis.ticks.x       = element_line(color = text_col, linewidth = 0.2),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),
    
    plot.margin        = margin(t = 10, r = 10, b = 10, l = 10),
    
    plot.title         = element_text(
      family           = 'title',
      color            = title_col,
      #face             = "bold",
      size             = 40,  
      margin           = margin(t = 5, b = 5)),
      
    plot.subtitle      = element_markdown(
      family           = 'subtitle',
      color            = title_col,
      lineheight       = 1.0, 
      #face             = "bold",
      size             = 12,
      margin           = margin(b = 10)),
      
    plot.caption       = element_markdown(
      family           = 'caption',
      color            = caption_col, 
      lineheight       = 0.6, 
      size             = 7,
      hjust            = 0.5, 
      halign           = 0.5,
      margin           = margin(t = 10, b = 5)),
  )  


# |- Annotated Plot ----   
p1 + 

  # annotations + arrows
  
  # Gardner Book 
  annotate("richtext", x = 1.6, y = 1.5, lineheight = 0.9, fill = NA,
           label.colour = NA, size = 2.4, color = col_palette[1], family = 'text',
           label = str_glue("Dorothea Rockburne<br>",
                            "Appearances: 1<br>",
                            "Coverage: 0.74 cm2")) +

  annotate("curve", x = 1.6, y = 1.2, xend = 1.8, yend = 0.77,
           arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
           curvature = -0.3, color = "gray72", size = 0.25) +
  
  annotate("richtext", x = 1.88, y = 2.78, lineheight = 0.9, fill = NA,
           label.colour = NA, size = 2.4, color = col_palette[2], family = 'text',
           label = str_glue("Pablo Picasso<br>",
                            "Appearances: 16<br>",
                            "Coverage: 2.15 cm2")) +
  
  annotate("curve", x = 1.9, y = 2.5, xend = 2.1, yend = 2.08,
           arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
           curvature = -0.3, color = "gray72", size = 0.25) +

  
  # Janson Book 
  annotate("richtext", x = 0.53, y = 1.61, lineheight = 0.9, fill = NA,
           label.colour = NA, size = 2.4, color = col_palette[1], family = 'text',
           label = str_glue("Hannah Höch<br>",
                            "Appearances: 2<br>",
                            "Coverage: 0.93 cm2")) + 
  
  annotate("curve", x = 0.55, y = 1.33, xend = 0.75, yend = 0.91,
           arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
           curvature = -0.3, color = "gray72", size = 0.25) +

  annotate("richtext", x = 0.94, y = 2.67, lineheight = 0.9, fill = NA, 
           label.colour = NA, size = 2.4, color = col_palette[2], family = 'text',
           label = str_glue("Pablo Picasso<br>",
                            "Appearances: 9<br>",
                            "Coverage: 3.27 cm2")) +

  annotate("curve", x = 0.94, y = 2.93, xend = 1.14, yend = 3.36, 
           arrow = arrow(length = unit(0.02, "npc"), type = "closed"), 
           curvature = 0.3, color = "gray72", size = 0.25) 

  

sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19044)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-07-14
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
# compiler       4.3.1   2023-06-16 [2] local
# curl           5.0.0   2023-01-12 [1] CRAN (R 4.3.0)
# datasets     * 4.3.1   2023-06-16 [2] local
# dplyr        * 1.1.2   2023-04-20 [1] CRAN (R 4.3.0)
# fansi          1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.2   2023-04-25 [1] CRAN (R 4.3.0)
# generics       0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
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
# lifecycle      1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.4   2023-03-09 [1] CRAN (R 4.3.0)
# magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
# methods      * 4.3.1   2023-06-16 [2] local
# munsell        0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
# pillar         1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.1   2023-01-10 [1] CRAN (R 4.3.0)
# R6             2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
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
# withr          2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
# xml2           1.3.4   2023-04-27 [1] CRAN (R 4.3.0)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
