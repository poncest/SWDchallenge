

# Challenge:  #SWDchallange 2023 -- August
## Data:      what does your audience know?
## Author:    Steven Ponce
## Date:      2023-08-02


## 0. DATA SOURCE ----
#' Impact of Swelling of Spray Dried Dispersions in Dissolution Media on their Dissolution: An Investigation Based on UV Imaging
#' Li 1, Christopher Zordan 2, Steven Ponce 2, Xujin Lu 2

#' Reference: 
#' J Pharm Sci . 2022 Jun;111(6):1761-1769. 
#' doi: 10.1016/j.xphs.2021.12.007. Epub 2021 Dec 9.
#' Link: https://pubmed.ncbi.nlm.nih.gov/34896344/


## 0. NOTES ----
#' For this month's challenge, I will focus on Figure 7. Swelling profiles in various media of IMC-HPMC-AS SDDs with different drug loading percentages. Polymer/drug matrix swelling (z distance, perpendicular to the x-axis)
#' is calculated by subtracting the z distance values at one absorbance unit of the 20 min timepoint minus 15 min timepoints. This represents a five-minute static hold at
#' 0 mL/min, 37.2 °C, 280 nm).
#' Version 1 - for exploratory data analysis. The assumption is that the audience knows the topic/subject well.
#' Version 2 - for a general audience which not necessarily know about the subject.


## Abbreviations ----
#' (IMC) indomethacin 
#' (HPMC-AS) hydroxypropyl methylcellulose acetate succinate 
#' (FaSSIF) fasted state simulated intestinal fluid 
#' (FeSSIF) fed state simulated intestinal fluid 


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, tidytuesdayR, ggtext, showtext, janitor, camcorder, scales, lubridate)
pacman::p_load(ggragged, patchwork)


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
raw_data <- read_csv("2023/08_August/fig7_polymer swelling profiles.csv") %>% 
  clean_names()

# 1AU absorbance data frame
abs_1au_data <- read_csv("2023/08_August/fig7_polymer swelling profiles_abs_1AU.csv") %>% 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(raw_data) 
colnames(raw_data)

raw_data$polymer_loading %>% unique()
raw_data$media %>% unique()


## 4. TIDYDATA ---- 

# |-  version 1 data ----

clean_data <- raw_data %>%
  # rename columns
  rename(
    pH = ph,
    z_distance = z_distance_x_1_mm,
    absorbance = abs_280_nm
  ) %>% 
  
  # convert to factors
  mutate(
    polymer_loading = as_factor(polymer_loading),
    polymer_loading = as_factor(drug_loading),
    media = as_factor(media),
    time_min = as_factor(time_min),
    pH = as_factor(pH)
  ) %>% 
  
  # add col for facet title
  mutate(factet_title = str_glue("{media}, pH {pH}")) %>% 
  
  # filter out end tail and water as disso media
  filter(z_distance <= 2,
         media != "Water")


# |-  version 2 data ----

abs_1au_data <- abs_1au_data %>%
  # rename columns
  rename(
    pH = ph,
    z_distance = z_distance_x_1_mm,
    absorbance = abs_280_nm
  ) %>% 
  
  # convert to factors
  mutate(
    polymer_loading = as_factor(polymer_loading),
    polymer_loading = as_factor(drug_loading),
    media = as_factor(media),
    time_min = as_factor(time_min),
    pH = as_factor(pH)
  ) %>% 
  
  # add col for facet title
  mutate(col_title = str_glue("Drug Loading: {drug_loading}%"),
         row_title = str_glue("{media}\ \n pH {pH}")) 
  
  

# 5. VISUALIZATION ---- 

# |-  version 1 plot ----

#' The assumption is that the audience knows the topic/subject well. 
#' Ideal for for exploratory data analysis. Quick plot for a project team meetings.


p1 <- clean_data %>% 
  ggplot(aes(x = z_distance, y = absorbance, color = time_min))+
  geom_line() +
  facet_wrap(vars(media, drug_loading), labeller = "label_both", ncol = 3)

p1


# |- plot aesthetics ---- 
bkg_col      <- "#fafafa"
title_col    <- "#1A181B"                 
subtitle_col <- "#1A181B"   
caption_col  <- "#1A181B"  
text_col     <- "#1A181B"   
col_palette  <- c("15" = "#625B66", "20" = "#625B66")


# |-  titles and caption ----
tt <- str_glue("#SWDchallenge: July 2023 &bull; Source: J Pharm Sci. 2022 Jun;111(6):1761-1769<br>")
tw <- str_glue("<span style='font-family:fa-brands'>&#xf099;</span>")
gh <- str_glue("<span style='font-family:fa-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa-brands'>&#xf4f6;</span>")

title_text <- str_glue("Swelling profiles in various media of IMC-HPMC-AS SDDs<br>with different drug loading percentages")

tp_15_min <- str_glue("<span style='font-size:10pt; color: #000000'>**15 min**</span>")
tp_20_min <- str_glue("<span style='font-size:10pt; color: #ff00ff'>**20 min**</span>")

subtitle_text <- str_glue("Polymer/drug matrix swelling (z distance, perpendicular to the x-axis) is calculated by subtracting<br>",
                          "the z distance values at one absorbance unit of the {tp_20_min} timepoint minus {tp_15_min} timepoints.<br>", 
                          "This represents a five-minute static hold at 0 mL/min, 37.2 °C, 280 nm)")

caption_text  <- str_glue("{tt} Visualization: {tw} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


# |-  fonts ----
font_add('fa-brands', 'fonts/fa-brands-400.ttf')
font_add_google("Lato", family = "title")                            
font_add_google("Source Sans Pro", family = "subtitle")              
font_add_google("Ruda", family = "text")                  
font_add_google("PT Sans Narrow", family = "caption")
showtext_auto(enable = TRUE)  



# |-  version 2 plot ----

# additional format
data_plot2 <- clean_data %>% 
  # add col for facet title
  mutate(col_title = str_glue("Drug Loading: {drug_loading}%"),
         row_title = str_glue("{media}\ \n pH {pH}")) 


main_plot <- data_plot2 %>% 
  ggplot(aes(x = z_distance, y = absorbance, color = time_min))+
  
  # geoms
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "red", lty = 3) +
  
  geom_point(data = abs_1au_data %>% filter(time_min == 15), 
             color = "black", size = 2) +
  
  geom_point(data = abs_1au_data %>% filter(time_min == 20), 
             color = "magenta", size = 2) +
  
  # scales
  scale_x_continuous(breaks = seq(0, 2, by = 0.5),
                     limits = c(0, 2),
                     expand = c(0.05, 0.05)) +
  
  scale_y_continuous(breaks = seq(0, 2, by = 1),
                   limits = c(0, 2),
                   expand = c(0.05, 0.05)) +
  
  coord_cartesian(clip = "off")+
  
  scale_color_manual(values = col_palette) +
  
  # labs
  labs(x = "Z Distance (mm)",
       y = "Abs (AU)",
       title    = title_text,
       subtitle = subtitle_text) +
  
  # facet
  facet_ragged_rows(
    vars(row_title),              #  rows - media
    vars(col_title),              # cols - drug loading
    labeller = "label_value",
    scales = "free_y")+

  # theme
  theme_minimal()+
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    
    
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.text          = element_text(color = text_col),
    
    axis.title.y       = element_text(color = text_col, face = 'bold', size = 12, 
                                      margin = margin(r = 10), hjust = 0.5),
   
    axis.title.x       = element_text(color = text_col, face = 'bold', size = 12, 
                                      margin = margin(t = 10), hjust = 0.5),
   
    axis.line.x        = element_line(color = text_col, linewidth = 0.2),
    axis.ticks.x       = element_line(color = text_col, linewidth = 0.2),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),
    
    plot.margin        = margin(t = 20, r = 20, b = 20, l = 20),
    
    strip.text         = element_textbox(size     = 10,
                                         face     = 'bold',
                                         color    = text_col,
                                         hjust    = 0.5,
                                         halign   = 0.5,
                                         r        = unit(5, "pt"),
                                         width    = unit(5.5, "npc"),
                                         padding  = margin(3, 0, 3, 0),
                                         margin   = margin(3, 3, 3, 3),
                                         fill     = "transparent"),
    
    panel.spacing       = unit(1, 'lines'),
    
    plot.title         = element_markdown(
      family           = 'title',
      color            = title_col,
      lineheight       = 1.0, 
      face             = "bold",
      size             = 18,  
      margin           = margin(t = 5, b = 5)),
    
    plot.subtitle      = element_markdown(
      family           = 'subtitle',
      color            = title_col,
      lineheight       = 1.0, 
      size             = 10,
      margin           = margin(b = 10)),
  )  

main_plot


# KEY
key <- data_plot2 %>% 
  filter(media == "Phosphate Buffer", drug_loading == 35) %>% 
  
  ggplot(aes(x = z_distance, y = absorbance, color = time_min))+
  
  # geoms
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  geom_hline(yintercept = 1, linewidth = 0.5, color = "red", lty = 3) +
  
  geom_point(data = abs_1au_data %>% filter(time_min == 15,
                                            media == "Phosphate Buffer", 
                                            drug_loading == 35), 
             color = "black", size = 2) +
  
  geom_point(data = abs_1au_data %>% filter(time_min == 20,
                                            media == "Phosphate Buffer", 
                                            drug_loading == 35), 
             color = "magenta", size = 2) +
  
  # scales
  scale_x_continuous(breaks = seq(0, 2, by = 0.5),
                     limits = c(0.1, 1.9)) +
  
  scale_y_continuous(breaks = seq(0, 2, by = 1),
                     limits = c(0, 2)) +
  
  coord_cartesian(clip = "off")+
  
  scale_color_manual(values = col_palette) +
  
  # labs
  labs(x = "", y = "", 
       subtitle = "KEY") +

  # theme
  theme_minimal()+
  theme(
    legend.position       = 'plot',
    
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
    
    axis.text          = element_blank(),
    axis.title         = element_blank(),

    axis.line          = element_blank(),
    axis.ticks         = element_blank(),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),
    
    plot.margin        = margin(t = 20, r = 20, b = 20, l = 20),
    
    plot.subtitle      = element_markdown(
      family           = 'subtitle',
      color            = title_col,
      lineheight       = 1.0, 
      face             = "bold",
      size             = 14,
      margin           = margin(b = 10)),
    )  

key

# Empty
empty <- ggplot()+
  theme_void()+
  theme(
    legend.position    = 'plot',
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
  )

# caption
caption <- ggplot()+
  theme_minimal()+
  labs(caption = caption_text) +
  theme(
    plot.background    = element_rect(fill = bkg_col, color = bkg_col),
    panel.background   = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.caption       = element_markdown(
      family           = 'caption',
      color            = caption_col, 
      lineheight       = 0.6, 
      size             = 8,
      hjust            = 0.5, 
      halign           = 0.5,
      valign           = 0,
      margin           = margin(t = 5, b = 5))
  )


## all plots together
# reference: https://patchwork.data-imaginist.com/reference/area.html

# grid area
design <- c(
  area(1,1,5,5),   # main
  area(6,1,6,5)    # key + empty + caption
)

main_plot / (key + empty + caption) + plot_layout(design = design)

## NOTES:
# Post-processing will occur in MS PowerPoint
# Need to insert the swelling formula for clarity


sessioninfo::session_info(include_base = TRUE) 
## 6. SESSION INFO ---- 
# ─ Session info ───────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2023-08-05
# rstudio  2023.06.1+524 Mountain Hydrangea (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────
# package      * version date (UTC) lib source
# base         * 4.3.1   2023-06-16 [2] local
# bit            4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
# bit64          4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
# camcorder    * 0.1.0   2022-10-03 [1] CRAN (R 4.3.0)
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
# ggplot2      * 3.4.2   2023-04-03 [1] CRAN (R 4.3.0)
# ggragged     * 0.1.0   2023-04-20 [1] CRAN (R 4.3.0)
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
# patchwork    * 1.1.2   2022-08-19 [1] CRAN (R 4.3.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────────
# > 
