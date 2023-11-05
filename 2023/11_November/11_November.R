

# Challenge:  #SWDchallange 2023 -- November
## Topic:     create a clever combo chart
## Data:      US Dept of Education
## Author:    Steven Ponce
## Date:      2023-11-05


## 0. DATA SOURCE ----
#' The data comes from { Pell grant } R package. The original source from US Dept of Education.



## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext)
pacman::p_load(janitor, here, skimr, glue, camcorder, scales, patchwork)



# |- figure size ---- 
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 15,
  units  = "in",
  dpi    = 320) 

# |- resolution ---- 
showtext_opts(dpi = 320)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(2022, week = 35) 

pell <- tt$pell %>% clean_names() %>% glimpse()

tidytuesdayR::readme(tt) 

rm(tt)   

# source helper functions
source("2023/11_November/functions/helper_functions.R")



## 3. EXAMINING THE DATA ----
glimpse(pell)
skim(pell)

unique(pell$state)
range(pell$year)



## 4. TIDYDATA ---- 

# |- focus on Puerto Rico (PR) ----
PR_pell <- pell %>% 
  filter(state == "PR") 


# |- award per recipient ----
PR_award_per_recipient_school_tbl <- PR_pell %>% 
  group_by(year, name) %>% 
  mutate(award_per_recipient = award / recipient) %>% 
  ungroup()


# |- median award per recipient (p1) ----
PR_median_award_per_recipient_year_tbl <- PR_pell %>% 
  group_summary(group_col = year) 
  
  
# |- start/end line points ----
years <- c("1999", "2017")

start_end_points_tbl  <- PR_median_award_per_recipient_year_tbl %>%     
  filter(year %in% years) 

# |- label text for p1 ----
label_text <- tibble(x = c(1999, 2017),
                     y = c(start_end_points_tbl$median_award_per_recipient[1] ,
                           start_end_points_tbl$median_award_per_recipient[2]),
                     hjust = c(0.5, 0.5),                                            
                     
                     year_text   = c(start_end_points_tbl$year[1], start_end_points_tbl$year[2]),
                     
                     amount_text = c(start_end_points_tbl$median_award_per_recipient[1],
                                     start_end_points_tbl$median_award_per_recipient[2])
)

# |- KPI (p2) ----
label_text <- label_text %>% 
  mutate(KPI_text = scales::percent(amount_text[1] / amount_text[2])) %>% 
  add_column(KPI_subtext = "(Not adjusted by cost of living)")


# |- median award per school (p3) ----
PR_schools_year_tbl <- PR_pell %>% 
  group_by(year, name) %>% 
  summary_only() %>% 
  
  group_by(year) %>% 
  mutate(
    total_median_recipient = sum(median_recipient),
    total_median_award     = sum(median_award)
  ) %>% 
  ungroup() %>% 
  
  mutate(
    recipient_pct = median_recipient / total_median_recipient,
    award_pct     = median_award / total_median_award
  )

#|- labels for p3 ----
# tibble for text
annotations_p3 <- tibble(x       = c(10664, 25343, 20453),
                         y       = c(199e6, 168e6, 145e6),
                         hjust   = c(0.6, 1, 0.4),
                         hjust_2 = c(0.6, 1.5, 0.4),           
                         vjust   = c(1, 1, 1),
                         vjust_2 = c(1, -0.3, 1),             
                         
                         title = c("University of Puerto Rico - Central Administration - Rio Piedras", "Inter American University of Puerto Rico", "University of Puerto Rico - Central Administration"),
                         
                         full_text = c("2009, $179 M", "2009, $157 M", "2008, $141 M"))


# tibble for vertical segments
label_segment_vertical_p3 <- tibble(x    = c(19664, 26664, 32964),
                                    xend = c(19664, 26664, 32964),
                                    y    = c(200e6, 170e6, 146e6),
                                    yend = c(179e6, 152e6, 125e6))


# tibble for horizontal segments
label_segment_horizontal_p3 <- tibble(x    = c(33664, 34664, 34964),
                                      xend = c(20664, 27664, 33664),
                                      y    = c(190e6, 160e6, 136e6),
                                      yend = c(190e6, 160e6, 136e6))

# tibble for arrows
label_arrows_p3 <- tibble(x    = c(33664, 34664, 34964),
                          xend = c(39664, 37364, 36664),
                          y    = c(190e6, 160e6, 136e6),
                          yend = c(180e6, 157e6, 139e6))




# 5. VISUALIZATION ---- 

# |- plot aesthetics ---- 
col_1        <- 'black'    
col_2        <- 'black'     
bkg_col      <- "#F7E371"    
title_col    <- 'black'
subtitle_col <- "black"
caption_col  <- "black"


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: November 2023 &bull; Source: Pell grant R package<br>")
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>") 
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text    <- str_glue("Pell Grants")

subtitle_text <- str_glue("Puerto Rico, 1999 - 2017")

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Bungee", family = "title")                        
font_add_google("Coda", family = "subtitle")            
font_add_google("Roboto Condensed", family = "text")                    
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



# |- Line chart for Median Pell grant per recipient in PR, 1999 - 2017 -----
line_plot <- PR_median_award_per_recipient_year_tbl %>% 
  ggplot(aes(x = year, y = median_award_per_recipient)) + 
  
  # geoms
  geom_line(linewidth = 2, color = col_2) + 
  
  geom_point(data = start_end_points_tbl, aes(x = year, y = median_award_per_recipient), size = 7, color = col_1) + 
  
  geom_text(data = label_text, aes(x = x, y = y + 800, label = scales::comma(round(amount_text), prefix = "$"), 
                                   hjust = hjust), vjust = 1, size = 10, fontface = "bold", color = col_1) + 
  
  geom_text(data = label_text, aes(x = x, y = y + 500, label = year_text, hjust = hjust), 
            vjust = 1, size = 10, fontface = "bold", color = col_1) +
  
  # scales
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip = "off") +
  
  # labs
  labs(
    title    = title_text,
    subtitle = subtitle_text
  ) +
  
  # theme
  theme_minimal(base_size   = 16, base_family = 'text') +
  
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
       
    axis.title            = element_blank(),
    axis.text             = element_blank(),
       
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_blank(),
       
    plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
       
    plot.title            = element_markdown(
      family              = 'title',
      color               = title_col,
      face                = "bold",
      size                = 50,  
      margin              = margin(t = 10)),
         
    plot.subtitle         = element_markdown(
      family              = 'subtitle',
      color               = title_col,
      lineheight          = 0.6, 
      face                = "bold",
      size                = 30,
      margin              = margin(t = 10, b = 30)),
  )

line_plot


# |- KPI plot ----
KPI_plot <- label_text %>% 
  ggplot() + 
  
  # geoms
  # triangle
  geom_point(aes(x = 3.1, y = 2.0), size = 10, shape = 17, color = col_2) +
  
  # big number
  geom_text(aes(x = 3.9, y = 5, label = KPI_text[1], hjust = hjust), 
            vjust = 1, size = 24, fontface = "bold", color = col_2) +
  
  # disclaimer 
  geom_text(aes(x = 3.65, y = 0.8, label = KPI_subtext[1], hjust = hjust), 
            vjust = 1, size = 4, fontface = "bold", color = col_2) +
  
  # scales
  scale_x_continuous(limits = c(0, 5))+
  scale_y_continuous(limits = c(0, 5))+
  
  # theme
  theme_void() 

KPI_plot


# |- Scatter plot median award vs. median recipient  -----
scatter_plot <- PR_schools_year_tbl %>% 
  ggplot(aes(x = median_recipient, y = median_award)) + 
  
  # geoms
  geom_point(size = 5, na.rm = TRUE, shape = 21, color = 'black', fill = 'gray20', alpha = 0.45) + 
  
  geom_smooth(method = 'lm', se = FALSE, linewidth = 0.6, linetype = 2, color = 'black' ) +
  
  # university label
  geom_text(data = annotations_p3, aes(x = x, y = y + 250, label = str_wrap(title, 40), hjust = hjust, vjust = vjust),
            size = 5, fontface = "bold", color = col_1) +
  
  # year and grant amount
  geom_text(data = annotations_p3, aes(x = x, y = y - 15e6, label = full_text, hjust = hjust_2, vjust = vjust_2),
            size = 8, fontface = "bold", color = col_1) +  
  
  # label segments
  geom_segment(data = label_segment_vertical_p3, aes(x=x, xend = xend, y = y,  yend = yend), color = col_1) +
  geom_segment(data = label_segment_horizontal_p3, aes(x=x, xend = xend, y = y,  yend = yend), color = col_1) +
  
  # label arrows
  geom_segment(data = label_arrows_p3, aes(x=x, xend = xend, y = y,  yend = yend),
               arrow = arrow(length = unit(2, "mm")), color = col_1) +
  
  # scales
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(0, 45000, by = 10000),
                     limits = c(-5, 45000)) +    
  
  scale_y_continuous(labels = dollar_format(suffix = " M", scale = 1e-6),
                     breaks = seq(0, 200e6, by = 50e6),
                     limits = c(0, 200e6)) +            
  
  # labs
  labs(
    x = "Median Recipients", y = "Median Award",
    caption  = caption_text) +
  
  # theme
  theme_minimal(base_size = 16, base_family = 'text') +
  theme(
    
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.title.x       = element_text(size = 16, face = 'bold', margin = margin (t = 10)),
    axis.title.y       = element_text(size = 16, face = 'bold', margin = margin (r = 10)),
    
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", size = 0.4, color = 'gray'),
    
    plot.margin        = margin(t = 10, r = 20, b = 10, l = 20),
    
    plot.caption       = element_markdown( 
      family           = 'caption',
      color            = caption_col, 
      lineheight       = 0.6, 
      size             = 12,
      hjust            = 0.5,
      margin           = margin(t = 20, b = 10)),
  )

scatter_plot

# |- Putting all plots together ---
top <- line_plot +  
  inset_element(KPI_plot, left = 0, bottom = 0, right = 1.02, top = 0.2)


final <- top / scatter_plot +
  plot_layout(heights = c(2, 3))

final


sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-11-05
# rstudio  2023.09.0+463 Desert Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [2] local
# base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.3.0)
# bit            4.0.5    2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5    2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# commonmark     1.9.0    2023-03-17 [1] CRAN (R 4.3.0)
# compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.2    2022-09-29 [1] CRAN (R 4.3.0)
# curl           5.0.2    2023-08-14 [1] CRAN (R 4.3.1)
# datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.1)
# dplyr        * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)
# fansi          1.0.4    2023-01-22 [1] CRAN (R 4.3.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.3.0)
# fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggplot2      * 3.4.3    2023-08-14 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue         * 1.6.2    2022-02-24 [1] CRAN (R 4.3.0)
# graphics     * 4.3.1    2023-06-16 [2] local
# grDevices    * 4.3.1    2023-06-16 [2] local
# grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# here         * 1.0.1    2020-12-13 [1] CRAN (R 4.3.0)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# htmltools      0.5.6    2023-08-10 [1] CRAN (R 4.3.1)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.7    2023-06-29 [1] CRAN (R 4.3.1)
# knitr          1.44     2023-09-11 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
# lifecycle      1.0.3    2022-10-07 [1] CRAN (R 4.3.0)
# lubridate    * 1.9.2    2023-02-10 [1] CRAN (R 4.3.0)
# magick         2.7.5    2023-08-07 [1] CRAN (R 4.3.1)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# markdown       1.8      2023-08-23 [1] CRAN (R 4.3.1)
# Matrix         1.6-1    2023-08-14 [2] CRAN (R 4.3.1)
# methods      * 4.3.1    2023-06-16 [2] local
# mgcv           1.9-0    2023-07-11 [2] CRAN (R 4.3.1)
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# nlme           3.1-163  2023-08-09 [2] CRAN (R 4.3.1)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# parallel       4.3.1    2023-06-16 [2] local
# patchwork    * 1.1.3    2023-08-14 [1] CRAN (R 4.3.1)
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# ragg           1.2.5    2023-01-12 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.4    2023-02-10 [1] CRAN (R 4.3.0)
# readxl         1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# repr           1.1.6    2023-01-26 [1] CRAN (R 4.3.1)
# rlang          1.1.1    2023-04-28 [1] CRAN (R 4.3.0)
# rprojroot      2.0.3    2022-04-02 [1] CRAN (R 4.3.0)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.4.0    2022-11-21 [1] CRAN (R 4.3.0)
# rvest          1.0.3    2022-08-19 [1] CRAN (R 4.3.0)
# scales       * 1.2.1    2022-08-20 [1] CRAN (R 4.3.1)
# selectr        0.4-2    2019-11-20 [1] CRAN (R 4.3.0)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# skimr        * 2.1.5    2022-12-23 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# splines        4.3.1    2023-06-16 [2] local
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
# tidytuesdayR * 1.0.2    2022-02-01 [1] CRAN (R 4.3.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.1    2023-06-16 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.3    2023-01-31 [1] CRAN (R 4.3.0)
# utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.6.3    2023-06-14 [1] CRAN (R 4.3.1)
# vroom          1.6.3    2023-04-28 [1] CRAN (R 4.3.0)
# withr          2.5.1    2023-09-26 [1] CRAN (R 4.3.1)
# xfun           0.40     2023-08-09 [1] CRAN (R 4.3.1)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.1/library
# 
# ───────────────────────────────────────────────────────────────────────────────────
# >
