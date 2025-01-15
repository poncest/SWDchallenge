
## Challenge: #SWDchallenge 2025-- January Exercise
## Topic:     Exercises | use space and alignment effectively
## Data:      Let's Practice! Exercise 3.7
## Author:    Steven Ponce
## Date:      2025-01-15


## 0. DATA SOURCE ----
#' 
#' This exercise is based on Let's Practice! Exercise 3.7. 
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1xiEL2Hdy-c13FB6SYJRQgtgjEKwbdrxb/edit?gid=813808745#gid=813808745
#' 


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  camcorder          # Record Your Plot History
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
raw_data <- read_csv("2025/Ex_037/lets_practice_exercise_037.csv") |> clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data )


## 4. TIDY ----
tidy_energy_drinks <- raw_data |>
  pivot_longer(
    cols = c(storm_fuel, neon_pulse, lime_rush),
    names_to = "brand",
    values_to = "score"
  ) |>
  mutate(
    brand = str_to_title(str_replace(brand, "_", " ")),
    # Reorder consumer likeability for more logical presentation
    consumer_likeability = factor(consumer_likeability,
      levels = c(
        "Won't buy", "Definitely buy", "Might buy",
        "Bold and exciting name", "Refreshing options",
        "Reliable energy boost", "Eye-catching design",
        "Reasonably priced", "Healthy energy source",
        "Supports an active day"
      )
    )
  )


## 5. VISUALIZATION ---- 

### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <-  c("#2c3e50", "#e74c3c", "#3498db")

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Jan 2025 Excercise &bull; Source: Source: Let's Practice! Exercise 3.7<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

title_text   <- str_glue("Energy Drink Brand Comparison: Activity Support Drives High Scores") 

subtitle_text <- str_glue("__'Supports an active day'__ ranks highest across all brands, while __health claims__ show largest variation")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

# |- fonts ----
font_add("fa6-brands", here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf"))
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "top",
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.9), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "#252525", linewidth = .3),
  axis.ticks.x          = element_line(color = text_col),  
  axis.title            = element_text(face = "bold"),
  panel.grid.minor      = element_blank(),
  panel.grid.major      = element_blank(),
  panel.grid.major.y    = element_line(color = "grey85", linewidth = .4),
)  

# |- plot ----
ggplot(
  data = tidy_energy_drinks,
  aes(x = score, y = consumer_likeability, group = consumer_likeability, color = brand)
  ) +
  
  # Geoms
  geom_line(color = "gray85", linewidth = 0.8) +
  geom_point(size = 3.5) +

  # Scales
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  scale_y_discrete() +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    x = "Score",
    y = NULL,
    color = "Brand: ",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) + 
  
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.72),
      family = "title",
      face = "bold",
      color = title_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(1.02),
      family = "subtitle",
      color = caption_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = "caption",
      color = caption_col,
      lineheight = 1.1,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 15, b = 5)
    )
  ) 

# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-01-15
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here        * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P rlang         1.1.4    2024-06-04 [?] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# P stringi       1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.1.0    2024-05-15 [?] CRAN (R 4.4.0)
# P textshaping   0.4.0    2024-05-24 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P tzdb          0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────
# > 
