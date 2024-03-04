
# Challenge:  #SWDchallenge 2024 -- March
## Topic:     design for accessibility
## Data:      Freedom in the World
## Author:    Steven Ponce
## Date:      2024-03-01

## REFERENCE ----
#' Paul Tol's Notes: Introduction to color schemes
#' 18 August 2022
#' Link: https://personal.sron.nl/~pault/#sec:qualitative


## 0. DATA SOURCE ----
#' 
#' Dataset 1:
#' World Freedom Index
#' Link: https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-22/readme.md
#'  
#' 
#' Dataset 2:
#' Freedom in the World
#' Link: https://freedomhouse.org/report/freedom-world
#' 
#' Data: All Data, FIW 2013-2024 (Excel Download)
#' Link: https://freedomhouse.org/sites/default/files/2024-02/All_data_FIW_2013-2024.xlsx
#' 


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(tidyverse, ggtext, showtext, camcorder, scales, janitor)
pacman::p_load(readxl, ggbump)


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

### |- freedom_1995_2020 ----

tt <- tidytuesdayR::tt_load(x = 2022, week = 8) 

freedom_1995_2020_raw <- tt$freedom |> clean_names() |> glimpse()

### |- freedom_2013_2024 ----

freedom_2013_2024_raw <- read_xlsx(path = "2024/03_Mar/All_data_FIW_2013-2024.xlsx", 
          sheet = "FIW13-24", 
          skip  = 1, 
          col_names = TRUE, 
          trim_ws   = TRUE,
          .name_repair = "unique") |> 
  clean_names() 


## 3. EXAMINE THE DATA ----
glimpse(freedom_1995_2020_raw)
colnames(freedom_1995_2020_raw)
skimr::skim(freedom_1995_2020_raw)

glimpse(freedom_2013_2024_raw)
colnames(freedom_2013_2024_raw)
skimr::skim(freedom_2013_2024_raw)



## 4. TIDY ----

### |- freedom_1995_2020 ----
freedom_1995_2020_df <- freedom_1995_2020_raw |>
  # Rename columns
  rename(
    cl_rating = cl,
    pr_rating = pr,
    region = region_name
  ) |> 
  # Remove columns
  select(-c(region_code, is_ldc)) |> 
  # 5-year column
  mutate(quinquennial = (year %/% 5) * 5) |>                                    # 5-years group
  group_by(country) |>
  # Calculate mean rating column
  mutate(mean_rating = rowMeans(cbind(cl_rating, pr_rating), na.rm = TRUE)) |> 
  ungroup() |> 
  # 5-year mean
  summarise(
    mean_rating = mean(mean_rating),
    .by = c('country', 'quinquennial')
  ) |>
  # Derive combined status
  mutate(
    combined_status = case_when(
      mean_rating   <= 2.5    ~ 'Free',
      mean_rating   > 2.5 &
        mean_rating < 5.1     ~ 'Partially Free',
      TRUE                    ~ 'Not Free'
    )) |>
  ungroup()
 

### |- freedom_2021_2024 ----
freedom_2021_2024_df <- freedom_2013_2024_raw |> 
  select(country_territory:cl_rating) |> 
  # Convert columns to factors
  mutate(across(country_territory:c_t, factor)) |> 
  # 5-year  column 
  mutate(quinquennial = (edition %/% 5) * 5) |>                                 # 5-years group
  group_by(country_territory) |>
  # Calculate mean rating column
  mutate(mean_rating = rowMeans(cbind(cl_rating, pr_rating), na.rm = TRUE)) |> 
  ungroup() |> 
  # 5-year mean
  summarise(
    mean_rating = mean(mean_rating),
    .by = c('country_territory', 'quinquennial')
  ) |>
  # Derive combined status
  mutate(
    combined_status = case_when(
      mean_rating   <= 2.5    ~ 'Free',
      mean_rating   > 2.5 &
        mean_rating < 5.1     ~ 'Partially Free',
      TRUE                    ~ 'Not Free'
    )) |>
  ungroup() |> 
  # Filter years 
  filter(quinquennial >= 2020) |> 
  # Rename columns
  rename(country = country_territory) 
 

### |- freedom_combined ----
freedom_combined_df <- bind_rows(list(
  freedom_1995_2020_df, 
  freedom_2021_2024_df), .id = "id") |> 
  arrange(country, quinquennial)  


### |- World df ----
world_df <- freedom_combined_df |>          
  summarise(
    mean_rating = mean(mean_rating, na.rm = TRUE),
    combined_status = case_when(
      mean_rating   <= 2.5   ~ 'Free',
      mean_rating   > 2.5 & 
        mean_rating < 5.1    ~ 'Partially Free',
      TRUE                   ~ 'Not Free'
    ), 
    .by = quinquennial 
  ) |> 
mutate(country = 'World')


### |-  freedom_world df ----
freedom_world_df <- bind_rows(
  freedom_combined_df, 
  world_df) 


# check country names
freedom_world_df$country |> unique() |> sort()

# south american countries
south_america_countries <-  c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                              "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                              "Uruguay", "Venezuela", "World")


### |-  South america df ----
americas_df <- freedom_world_df |>
  # Format country names (south america)
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)"    ~ "Bolivia",
    country == "Venezuela (Bolivarian Republic of)"  ~ "Venezuela",
    TRUE ~ as.character(country)
  )) |> 
  # Select south america countries
  filter(country %in% south_america_countries)


### |- Country labels ----
country_labels <- americas_df |> 
  filter(
    country %in% south_america_countries,
    quinquennial == 2020
  )



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#fbf7f0', 0.65) 
title_col    <- "gray10"              
subtitle_col <- "gray20"   
caption_col  <- "gray20"  
text_col     <- "gray20" 

col_palette  <- c("#BBBBBB", "#BB5566", "#004488")                              # High-contrast qualitative color scheme


### |-  titles and caption ----
tt <- str_glue("#SWDchallenge: March 2024 &bull; Source: freedomhouse.org<br>")  
X  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")   
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

title_text <- str_glue("Freedom in South America, 1995-2024")

venezuela  <- str_glue("<span style='color:{ col_palette[2] }'>**Venezuela**</span>")

subtitle_text <- str_glue("The state of political rights and civil liberties in { venezuela } is deteriorating.<br><br>
                          <span style='font-size:10pt'>Median combined rating</span>")

caption_text  <- str_glue("{tt} Visualization: {X} @sponce1 &bull; {mn} @sponce1(graphic.social)<br>",
                          "Code: {gh} poncest &bull; Tools: #rstats #ggplot2")


### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Squada One", regular.wt = 400, family = "title")                            
font_add_google("Inter", family = "subtitle")   
font_add_google("Barlow Condensed", family = "text")  
font_add_google("Roboto Condensed", family = "caption")
showtext_auto(enable = TRUE)  


### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  
  axis.text             = element_text(size = rel(1), color = text_col, family   = 'text'),
  
  axis.line.x           = element_line(color = "black", linewidth = .2),
  
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.3, color = 'gray'),
)


### |-  labels for y-axis ----
label_text = c("", "", "", "", 'Free', "", 
               "","", 'Partially Free', "", "",
               "", "", 'Not Free', ""
               )


### |-  final plot ----
americas_df |> 
  ggplot(aes(x = quinquennial, y = mean_rating, color = country, 
             group = country, label = country) ) +
  
  # Geoms
  # Other south america countries
  geom_bump(smooth = 15, size = .2, color = col_palette[1]) +
  geom_point(size = 2, color = col_palette[1]) +

  # Venezuela
  geom_bump(data = americas_df |> filter(country == "Venezuela"),
            smooth = 15, size = .5, color = col_palette[2]) +
  geom_point(data = americas_df |> filter(country == "Venezuela"),
             size = 3, color = col_palette[2]) +
  geom_text(data = country_labels |> filter(country == "Venezuela"),             
            hjust = 0, nudge_x = 0.5, check_overlap = TRUE,
            fontface = 'bold', color = col_palette[2]) +
  
  # World
  geom_bump(data = americas_df |> filter(country == "World"),   
            smooth = 15, size = .5, color = col_palette[3]) +
  geom_point(data = americas_df |> filter(country == "World"),
             size = 3, color = col_palette[3]) +
  geom_text(data = country_labels |> filter(country == "World"),
            hjust = 0, nudge_x = .5, check_overlap = TRUE,
            fontface = 'bold', color = col_palette[3]) +
  
  # Segment on the y-axis
  annotate(geom = "segment", x = 1994, xend = 1994, y = 1, yend = 2.5, size = .2, color = 'gray60') +
  annotate(geom = "segment", x = 1994, xend = 1994, y = 3, yend = 5, size = .2, color = 'gray60') +
  annotate(geom = "segment", x = 1994, xend = 1994, y = 5.5, yend = 7, size = .2, color = 'gray60') +
  
  # Scales
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.16))) +
  scale_y_reverse(breaks = seq(0, 7, by = .5),                                   
                  labels = label_text) +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off") +
  
  # Labs
  labs(x        = "",
       y        = "",       
       title    = title_text,
       subtitle = subtitle_text,
       caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title    = element_text(
      size        = rel(2.4), 
      family      = 'title',
      face        = 'bold',
      color       = title_col,
      margin      = margin(t = 5, b = 5)), 
    
    plot.subtitle = element_markdown(
      size        = rel(.9), 
      family      = 'subtitle',
      color       = title_col,
      lineheight  = 1.1, 
      margin      = margin(t = 5, b = 10)),  
    
    plot.caption  = element_markdown(
      size        = rel(.65), 
      family      = 'caption',
      color       = caption_col,
      lineheight  = 0.65,
      hjust       = 0.5,
      halign      = 0.5,
      margin      = margin(t = -10, b = 5)),
  )



## 6. SESSION INFO ---- 
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-03-04
# rstudio  2023.12.1+402 Ocean Storm (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────
# package      * version  date (UTC) lib source
# base         * 4.3.2    2023-10-31 [2] local
# camcorder    * 0.1.0    2022-10-03 [1] CRAN (R 4.3.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.0)
# cli            3.6.1    2023-03-23 [1] CRAN (R 4.3.0)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.0)
# compiler       4.3.2    2023-10-31 [2] local
# curl           5.2.0    2023-12-08 [1] CRAN (R 4.3.2)
# datasets     * 4.3.2    2023-10-31 [2] local
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.3.2)
# fansi          1.0.6    2023-12-08 [1] CRAN (R 4.3.2)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.3.0)
# fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.1)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.0)
# ggbump       * 0.1.0    2020-04-24 [1] CRAN (R 4.3.1)
# ggplot2      * 3.5.0    2024-02-23 [1] CRAN (R 4.3.2)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.0)
# gifski         1.12.0-2 2023-08-12 [1] CRAN (R 4.3.1)
# glue           1.7.0    2024-01-09 [1] CRAN (R 4.3.2)
# graphics     * 4.3.2    2023-10-31 [2] local
# grDevices    * 4.3.2    2023-10-31 [2] local
# grid           4.3.2    2023-10-31 [2] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.0)
# gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.1)
# hms            1.1.3    2023-03-21 [1] CRAN (R 4.3.0)
# httr           1.4.7    2023-08-15 [1] CRAN (R 4.3.1)
# janitor      * 2.2.0    2023-02-02 [1] CRAN (R 4.3.0)
# jsonlite       1.8.8    2023-12-04 [1] CRAN (R 4.3.2)
# lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
# lubridate    * 1.9.3    2023-09-27 [1] CRAN (R 4.3.2)
# magick         2.8.2    2023-12-20 [1] CRAN (R 4.3.2)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.0)
# methods      * 4.3.2    2023-10-31 [2] local
# munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.0)
# pacman         0.5.1    2019-03-11 [1] CRAN (R 4.3.0)
# pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.0)
# purrr        * 1.0.2    2023-08-10 [1] CRAN (R 4.3.1)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.0)
# Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.1)
# readr        * 2.1.5    2024-01-10 [1] CRAN (R 4.3.2)
# readxl       * 1.4.3    2023-07-06 [1] CRAN (R 4.3.1)
# rlang          1.1.3    2024-01-10 [1] CRAN (R 4.3.2)
# rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.1)
# rsvg           2.6.0    2023-10-08 [1] CRAN (R 4.3.2)
# rvest          1.0.4    2024-02-12 [1] CRAN (R 4.3.2)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.0)
# showtext     * 0.9-6    2023-05-03 [1] CRAN (R 4.3.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.1)
# stats        * 4.3.2    2023-10-31 [2] local
# stringi        1.7.12   2023-01-11 [1] CRAN (R 4.3.0)
# stringr      * 1.5.0    2022-12-02 [1] CRAN (R 4.3.0)
# svglite        2.1.1    2023-01-10 [1] CRAN (R 4.3.0)
# sysfonts     * 0.8.8    2022-03-13 [1] CRAN (R 4.3.0)
# systemfonts    1.0.5    2023-10-09 [1] RSPM (R 4.3.0)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.3.2)
# tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.0)
# tidytuesdayR   1.0.3    2023-12-13 [1] CRAN (R 4.3.2)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.0)
# timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.0)
# tools          4.3.2    2023-10-31 [2] local
# tzdb           0.4.0    2023-05-12 [1] CRAN (R 4.3.0)
# usethis        2.2.2    2023-07-06 [1] CRAN (R 4.3.1)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
# utils        * 4.3.2    2023-10-31 [2] local
# vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
# withr          3.0.0    2024-01-16 [1] CRAN (R 4.3.2)
# xml2           1.3.5    2023-07-06 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Users/poncest/AppData/Local/Programs/R/R-4.3.2/library
# 
# ─────────────────────────────────────────
# > 
