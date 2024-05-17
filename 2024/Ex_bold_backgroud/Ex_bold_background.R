


## Challenge: STWD | graphing on a bold background
## Data:      Meals served over time
## Author:    Steven Ponce
## Date:      2024-05-17


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  lubridate,   # Make Dealing with Dates a Little Easier
  glue,        # Interpreted String Literals
  ggforce      # Accelerating 'ggplot2'
)                                   

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 4,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----

# Initial Plot: https://docs.google.com/presentation/d/1Iw9ziH4_Pmvgz39DShQOCpxQCOi9DsDU/edit#slide=id.p1

meals_data <- read_csv('2024/Ex_bold_brackgroud/data.csv') |> 
  clean_names() |> glimpse()


## 3. EXAMINING THE DATA ----
glimpse(meals_data)
skim(meals_data)
colnames(meals_data)


## 4. TIDYDATA ----

### |- plot data ----
plot_data <- meals_data |>
  mutate(
    yoy_change = (meals_served - lag(meals_served)) / lag(meals_served) * 100
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- "#323238" 
title_col    <- "#d7d7d8"             
subtitle_col <- "#d7d7d8"   
caption_col  <- "#d7d7d8"   
text_col     <- "#d7d7d8" 
color_1      <- "#8AA6AC"
color_2      <- "#DA8988"

### |-  titles and caption ----
tt <- str_glue("#SWDchallenge: 2024 Exercise: graphing on a bold background &bull; Source: Storytelling with Data: Lets Practice<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

last_year     <- str_glue("<span style='color:{ color_2 }'>**2023**</span>")

title_text    <- str_glue("Year-over-Year (YoY) Growth in Meals Served")

subtitle_text <- str_glue("27% Increase in { last_year } Compared to 2022 — Analyzing Annual Campaign Performance<br><br>
                          **YoY Percentage Change**") 

caption_text  <- str_glue("{tt} Visualization: {li} @sponce1 &bull; {mn} @sponce1(graphic.social) Code: {gh} poncest &bull; Tools: #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Goldman", regular.wt = 400, family = "title")  
font_add_google("Smooch Sans", family = "subtitle")  
font_add_google("Smooch Sans", family = "text")  
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
  axis.text             = element_text(size = rel(1), color = text_col, family = 'text'),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)


### |-  final plot ----  
plot_data |>
  ggplot(aes(x = campaign_year, y = yoy_change)) +
  
  # Geoms
  geom_hline(yintercept = 0, linetype = 1, linewidth = 0.3, color = "#d7d7d8") +
  geom_segment(aes(x = campaign_year, xend = campaign_year, y = 0, yend = yoy_change),
               position = position_dodge(width = 0.7),
               linewidth = 0.2,
               na.rm = TRUE,
               color = color_1
  ) +
  geom_segment(
    data = plot_data |> filter(campaign_year == 2023),
    aes(x = campaign_year, xend = campaign_year, y = 0, yend = yoy_change),
    position = position_dodge(width = 0.7),
    linewidth = 0.2,
    na.rm = TRUE,
    color = color_2
  ) +
  geom_point(aes(size = yoy_change),
             position = position_dodge(width = 0.7),
             na.rm = TRUE,
             color = color_1
  ) +
  geom_point(
    data = plot_data |> filter(campaign_year == 2023),
    aes(size = yoy_change),
    position = position_dodge(width = 0.7),
    color = color_2
  ) +
  annotate(
    "text", x = 2023, y = 60, 
    label = "2023: +27%", 
    size = 4, 
    hjust = 0.55,
    family = 'text',
    fontface = "bold",
    color  = color_2,
  ) +
  
  # scales
  scale_x_continuous(breaks = seq(2012, 2023, by = 2),  
                     limits = c(2012, 2023),
  ) +
  scale_y_continuous(breaks = seq(-50, 250, by = 50),   
                     limits = c(-50, 250),
                     labels = scales::label_number(
                       suffix = '<span style="font-size:10px;"> %</span>'
                     )
  ) +
  scale_size_continuous(range = c(1, 6), guide = "none") +
  coord_cartesian(clip = 'off') +
  
  # Labs
  labs(
    x = "Campaign Year",
    y = NULL,
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    axis.text.y   = ggtext::element_markdown(),
    
    plot.title    = element_text(
      size        = rel(1.3), 
      family      = 'title',
      face        = 'bold',
      color       = title_col,
      margin      = margin(t = 5, b = 5)), 
    
    plot.subtitle = element_markdown(
      size        = rel(1), 
      family      = 'subtitle',
      color       = title_col,
      lineheight  = 1.1, 
      margin      = margin(t = 5, b = 10)),  
    
    plot.caption  = element_markdown(
      size        = rel(.45), 
      family      = 'caption',
      color       = caption_col,
      lineheight  = 0.65,
      hjust       = 0.5,
      halign      = 0.5,
      margin      = margin(t = 10, b = 5)),
  )



# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ──────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-05-17
# rstudio  2024.04.1+748 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# base        * 4.4.0    2024-04-24 [2] local
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.2    2023-12-11 [?] CRAN (R 4.4.0)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggforce     * 0.4.2    2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.7.0    2024-01-09 [?] CRAN (R 4.4.0)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here          1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12     2023-12-06 [?] CRAN (R 4.4.0)
# P MASS          7.3-60.2 2024-04-24 [?] local
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P polyclip      1.10-6   2023-09-27 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.3    2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr       * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# P stringi       1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P styler        1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.0.6    2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping   0.3.7    2023-10-09 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tweenr        2.0.3    2024-02-26 [?] CRAN (R 4.4.0)
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/SWDchallenge/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────────
# > 