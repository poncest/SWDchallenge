
# Challenge:  #SWDchallenge 2024 -- May
## Topic:     MAY 2024 | when every point matters
## Author:    Steven Ponce
## Date:      2024-05-02


## 0. DATA SOURCE ----

## Data:      Family Guy Dataset (via Sourav Banerjee) 
##            Navigating the Quirky Universe of Family Guy: A Comprehensive Dataset
## Link:      https://www.kaggle.com/datasets/iamsouravbanerjee/family-guy-dataset


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
  MetBrewer,   # Color Palettes Inspired by Works at the Metropolitan Museum of Art
  MoMAColors,  # Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City
  ggbeeswarm,  # Categorical Scatter (Violin Point) Plots
  ggforce,     # Accelerating 'ggplot2'
  patchwork    # The Composer of Plots
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 6,
  height = 8,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
family_guy_data <- read_csv(here::here("2024/05_May/Family_Guy_Dataset.csv")) |>
  clean_names() |>
  glimpse()


## 3. EXAMINING THE DATA ----
glimpse(family_guy_data)
skim(family_guy_data)
colnames(family_guy_data)


## 4. TIDYDATA ----

### |- Tidy ----
family_guy_tidy <- family_guy_data |>
  rename(
    imdb_rating = im_db_rating,
    us_viewers_millions = u_s_viewers_millions
  ) |>
  mutate(
    imdb_rating         = as.numeric(imdb_rating),
    us_viewers_millions = as.numeric(us_viewers_millions),
    original_air_date   = mdy(original_air_date),
    year                = year(original_air_date),
    highlight           = case_when(
      us_viewers_millions >= 10.0 ~ "yes",
      TRUE ~ "no"
    )
  ) |> 
  filter(!is.na(year))

### |- Plot data ----
plot_data <- family_guy_tidy |> 
  select(
    original_air_date, year, season, no_of_episode_season, title_of_the_episode,
    imdb_rating, us_viewers_millions, highlight
    ) |> 
  mutate(
    season_year = paste(season, year, sep = "_"),
    season_year = factor(season_year)
    ) 


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "#3d3d3d"           
subtitle_col <- "#3d3d3d"     
caption_col  <- "gray20"   
text_col     <- colorspace::darken("#8e8a7b" , 0.2)    

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: April 2024 &bull; Source: Family Guy Dataset (via Sourav Banerjee)<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text   <- str_glue("Decoding Popularity Trends of Family Guy Across Two Decades") 

subtitle_text <- str_glue("Exploring how viewership and IMDb ratings have evolved across 21 seasons,<br> 
                          highlighting key episodes and trends in audience engagement.")

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("Oswald", regular.wt = 400, family = "title")                 
font_add_google("Quattrocento Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Quattrocento Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
custom_theme <- function() {
  theme_minimal(base_size = 12, base_family = "text") +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position       = "plot",
      plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
      plot.background       = element_rect(fill = bkg_col, color = bkg_col),
      panel.background      = element_rect(fill = bkg_col, color = bkg_col),
      axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
      axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(.9), color = text_col, family = "text", face = "bold"),
      axis.text             = element_text(size = rel(.8), color = text_col, family = "text"),
      panel.grid.major.y    = element_line(linetype = "solid", linewidth = 0.1, color = "gray"),
      panel.grid.minor      = element_blank()
    )
}

### |-  plot function ----
create_beeswarm_plot <- function(data, x_var, y_var, title_text) {
  
  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  
  ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    geom_beeswarm(cex = 2, size = 1.5, shape = 21) +
    geom_smooth(color = "red", linewidth = 0.5) +
    scale_x_continuous() +
    scale_y_continuous(labels = number_format(suffix = " M")) +
    coord_cartesian(clip = 'off') +
    labs(x = "Season", y = title_text) +
    custom_theme()  
}

### |-  viewers across season plot ---- 
p1 <- create_beeswarm_plot(plot_data, "season", "us_viewers_millions", "US Viewers") +
  geom_point(data = plot_data |> filter(us_viewers_millions > 20)) +
  
  ggforce::geom_mark_circle(
    data = plot_data |> filter(us_viewers_millions > 20),
    aes(
      x0 = 12,
      y0 = 17,
      label = "S1Ep1 - Death Has a Shadow, 1999", 
      description = str_glue("22M US viewers and 7.7 IMDb rating")
    ),
    expand = 0.05, 
    label.buffer   = unit(1, 'mm'),
    color          = "gray70",
    fill           = "transparent",
    label.fill     = bkg_col,
    label.fontsize = 8,
    label.family   = "text",
    con.type       = "elbow",
    con.size       = 0.3,
  ) 

### |-  ratings across season plot ----  
p2 <- create_beeswarm_plot(plot_data, "season", "imdb_rating", "IMDb Rating") +
  geom_point(data = plot_data |> filter(imdb_rating > 9)) +
  
  ggforce::geom_mark_circle(
    data = plot_data |> filter(imdb_rating > 9),
    aes(
      x0 = 16,
      y0 = 9.5,
      label = "S8Ep1 - Road to the Multiverse, 2009",
      description = str_glue("10.17M US viewers and 9.1 IMDb rating")
    ),
    expand = 0.05,
    color          = "gray70",
    label.fill     = "transparent",
    label.fontsize = 8,
    label.family   = "text",
    con.size       = 0.3,
    con.type       = "elbow",
    label.buffer   = unit(0, 'mm'),
  ) +
  
  geom_point(data = plot_data |> filter(imdb_rating < 4.2)) +
  
  ggforce::geom_mark_circle(
    data = plot_data |> filter(imdb_rating < 4.2),
    aes(
      x0 = 5.1,
      y0 = 4.6,
      label = "S17Ep16 - You Can't Handle the Booth!, 2019",
      description = str_glue("2.01M US viewers and 4.1 IMDb rating")
    ),
    expand = 0.05,
    color          = "gray70",
    fill           = "transparent",
    label.fill     = bkg_col,
    label.fontsize = 8,
    label.family   = "text",
    con.type       = "elbow",
    label.buffer   = unit(1, 'mm'),
  ) +
  
  scale_y_continuous(
    name = "IMDb Rating",
    limits = c(4, 10),  
    breaks = seq(4, 10, 1)  
  )

### |-  main plot ----  
main_plot <- p1 / p2

### |-  title plot ----  
title_plot <- ggplot() + 
  theme_void() + 
  
  # labs
  labs(
    title    = title_text,
    subtitle = subtitle_text
  ) +
  
  # Theme
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "plot",
    
    plot.margin           = margin(t = 5, r = 0, b = 5, l = 0),        
    plot.background       = element_rect(fill = bkg_col, color = bkg_col),
    panel.background      = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.title            = element_markdown(
      size                = rel(1.4),
      family              = "title",
      face                = "bold",
      color               = title_col,
      lineheight          = 1.1,
      margin              = margin(t = 5, b = 5)
    ),        
    plot.subtitle         = element_markdown(
      size                = rel(0.95), 
      family              = 'subtitle',
      color               = subtitle_col,
      lineheight          = 1.4, 
      margin              = margin(t = 5, b = 1)
    ))


### |-  caption plot ---- 
caption_plot <- ggplot() + 
  theme_void() + 
  
  # labs
  labs(caption = caption_text) +
  
  # Theme
  theme(
    plot.background  = element_rect(fill = bkg_col, color = bkg_col),
    panel.background = element_rect(fill = bkg_col, color = bkg_col),
    
    plot.caption = element_markdown(
      size       = rel(.55),
      family     = "caption",
      color      = caption_col,
      lineheight = 1.1,
      hjust      = 0.5,
      halign     = 0.5,
      margin     = margin(t = 5, b = 5)
    ))

### |-  final plot ----  
final_plot <- (title_plot / main_plot / caption_plot) +
  plot_layout(nrow = 3, heights = c(0.01, 1, 0.01))

final_plot 


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-05-02
# rstudio  2024.04.0+735 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# base        * 4.4.0      2024-04-24 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P beeswarm      0.4.0      2021-06-01 [?] CRAN (R 4.4.0)
# P bit           4.0.5      2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5      2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.2      2023-12-11 [?] CRAN (R 4.4.0)
# colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0      2024-04-24 [?] local
# cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
# P crayon        1.5.2      2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0      2024-04-24 [?] local
# P digest        0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1      2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap       1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P ggbeeswarm  * 0.7.2      2023-04-29 [?] CRAN (R 4.4.0)
# P ggforce     * 0.4.2      2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# ggstream      0.1.0      2021-05-06 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2      2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2   2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.7.0      2024-01-09 [?] CRAN (R 4.4.0)
# P graphics    * 4.4.0      2024-04-24 [?] local
# P grDevices   * 4.4.0      2024-04-24 [?] local
# P grid          4.4.0      2024-04-24 [?] local
# P gridtext      0.1.5      2022-09-16 [?] CRAN (R 4.4.0)
# gtable        0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
# P here          1.0.1      2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3      2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1    2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling      0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P lattice       0.22-6     2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P MASS          7.3-60.2   2024-04-24 [?] local
# P Matrix        1.7-0      2024-03-22 [?] CRAN (R 4.4.0)
# P MetBrewer   * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0      2024-04-24 [?] local
# P mgcv          1.9-1      2023-12-21 [?] CRAN (R 4.4.0)
# MoMAColors  * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# munsell       0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P nlme          3.1-164    2023-11-27 [?] CRAN (R 4.4.0)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0      2024-04-24 [?] local
# P patchwork   * 1.2.0      2024-01-08 [?] CRAN (R 4.4.0)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P polyclip      1.10-6     2023-09-27 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# renv          1.0.7      2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.3      2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot     2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr       * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P splines       4.4.0      2024-04-24 [?] local
# P stats       * 4.4.0      2024-04-24 [?] local
# P stringi       1.8.3      2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P svglite       2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.0.6      2024-03-07 [?] CRAN (R 4.4.0)
# P textshaping   0.3.7      2023-10-09 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0      2024-04-24 [?] local
# P tweenr        2.0.3      2024-02-26 [?] CRAN (R 4.4.0)
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0      2024-04-24 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P vipor         0.4.7      2023-12-18 [?] CRAN (R 4.4.0)
# P vroom         1.6.5      2023-12-05 [?] CRAN (R 4.4.0)
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/SWDchallenge/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/ebed5364
# 
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────
# > 
