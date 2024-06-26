
# Challenge:  #SWDchallenge 2024 -- June
## Topic:     Exercises | make the point clear
## Author:    Steven Ponce
## Date:      2024-06-26


## 0. DATA SOURCE ----

## Data:       
##            https://community.storytellingwithdata.com/exercises/make-the-point-clear
##            https://docs.google.com/presentation/d/1KKDe4lHp4tL3IDhX0MCXU4iRVeGPxVjZ/edit#slide=id.p1


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
  marquee,     # Markdown Parser and Renderer for R Graphics
  patchwork    # The Composer of Plots
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  =  11.25,
  height =  5.9,
  units  = "in",
  dpi    = 320) 

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ THE DATA ----
data_raw <- read_csv('2024/Ex_051/data_ex_051.csv') |> 
  clean_names() |> glimpse()


## 3. EXAMINING THE DATA ----
glimpse(data_raw)


# 4. TIDY DATA ---- 
data_clean <- data_raw |> 
  rename(region_country = column1) |> 
  mutate(
    across(
         c(last_year, this_year, annualized_growth), parse_number
         ),
    annualized_growth = annualized_growth / 100
      )

glimpse(data_clean)

### |- Dot Plot Data ----
data_long <- data_clean |> 
  pivot_longer(
    cols = c(last_year, this_year), 
    names_to = "year", 
    values_to = "sales"
  ) |> 
  # Add row numbers to ensure correct path
  mutate(
    order = ifelse(year == "last_year", 1, 2),
    region_country = fct_reorder(region_country, sales)
  )


# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- '#FAFAFA'   
title_col    <- "#3d3d3d"           
subtitle_col <- "#3d3d3d"     
caption_col  <- "gray30"   
text_col     <- "#3d3d3d"    
col_palette  <- MoMAColors::moma.colors(palette_name = "Andri", n = 3, type = "discrete")


### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Excercise 051 &bull; Source: Storytelling with Data: Let's Practice!<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text    <- "Regional Sales Performance"

subtitle_text <- "{#f56455 _Last Year_} vs. {#15134b _This Year_}"

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

note_text     <- str_glue("Notable Decrease in NW Region Sales\n 
                          Overall Growth in Other Regions & Countries")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("Oswald", regular.wt = 400, family = "title")                 
font_add_google("Quattrocento Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Quattrocento Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
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
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold', hjust = 1),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold', hjust = 1),
  axis.text.y           = element_text(size = rel(1), color = text_col, family = 'text'),
  axis.text.x           = element_blank(),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.15, color = 'gray40'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
  axis.line.x           = element_line(color = "#d7d7d8", linewidth = .2),
)

## |- Dot Plot with geom_path and arrows ----
p1 <- data_long |>
  ggplot(aes(x = region_country, y = sales, group = region_country)) +
  
  # Geoms
  geom_path(
    arrow = arrow(
      type = "closed",
      length = unit(0.09, "inches")
    ),
    color = "gray30"
  ) +
  geom_point(aes(color = year), alpha = 0.85, size = 1.2) +
  geom_text(
    data = data_long |> filter(year == "last_year"),
    aes(label = dollar(sales, scale = 1, suffix = " B")),
    color = col_palette[1],
    nudge_x = 0.3,
    size = 2.2
  ) + 
  geom_text(
    data = data_long |> filter(year == "this_year"),
    aes(label = dollar(sales, scale = 1, suffix = " B")),
    color = col_palette[2],
    nudge_x = -0.3,
    size = 2.2
  ) + 
 
  # Scales
  scale_x_discrete() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  coord_flip(clip = "off") +
  scale_color_manual(values = col_palette) +
  
  # Labs
  labs(
    x = "Region/Country",
    y = "Sales USD (in billions)",
    title = title_text,
    subtitle = subtitle_text,
  ) +
  
  # Theme
  theme(
    plot.title    = element_text(
      size        = rel(1.8),
      family      = "title",
      color       = title_col,
      face        = "bold",
      lineheight  = 0.85,
      margin      = margin(t = 5, b = 0)
    ),
    plot.subtitle = element_marquee(
      size        = rel(1.1),
      family      = "subtitle",
      color       = title_col,
      lineheight  = 0.85,
      margin      = margin(t = 0, b = 5)
    ),
  )


### |- Annotated Plot ----
arrow_plot <- p1 +
  annotate(
    "text",
    x = 6.2, y = 0, 
    label = note_text,
    hjust = 0.05, 
    color = title_col,
    size = 3, 
    lineheight  = 0.5,
    family = "text",
    fontface = 'bold'
  ) 


### |-  plot theme ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))    

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = 'plot',
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold', hjust = 1),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold', hjust = 1),
  axis.text             = element_text(size = rel(1), color = text_col, family = 'text'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  axis.line.x           = element_line(color = "#d7d7d8", linewidth = .2),
)


# text
title_text    <- "Annualized Growth Rates"

note_text_2   <- str_glue("The NW Region Experiences a Decline\n
                           Admist Overall Growth")

note_text_3   <- str_glue("Mexico and Cananda Lead the Way\n 
                           with Significant Growth")

## |- Lollipop Chart ----
p2 <- data_clean |>
  ggplot(aes(x = region_country, y = annualized_growth)) +
  
  # Geom
  geom_hline(yintercept = 0, linetype = 1, linewidth = 0.3, color = "#d7d7d8") +  
  geom_segment(aes(x = region_country, xend = region_country, y = 0, yend = annualized_growth), color = "grey") +
  geom_point(aes(size = annualized_growth), color = col_palette[3]) +
  
  # Scale
  scale_x_discrete() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  coord_flip(clip = 'off') +
  
  # Labs
  labs(
    title = title_text,
    x = "Region/Country",
    y = "Annualized Growth"
  ) +
  
  # Theme
  theme(
    plot.title    = element_text(
      size        = rel(1.6),
      family      = "title",
      color       = title_col,
      face        = "bold",
      lineheight  = 0.85,
      margin      = margin(t = 5, b = 20)
    ),
  )

### |- Annotated Plot ----
lollipop_plot <- p2 +
  annotate(
    "text",
    x = 5, y = 0.02, 
    label = note_text_2,
    hjust = 0.05, 
    color = title_col,
    size = 3, 
    lineheight  = 0.5,
    family = "text",
    fontface = 'bold'
  ) +
  annotate(
    "text",
    x = 2.5, y = 0.02, 
    label = note_text_3,
    hjust = 0.05, 
    color = title_col,
    size = 3, 
    lineheight  = 0.5,
    family = "text",
    fontface = 'bold'
  ) 
 
### |-  Combine plots using patchwork ----
combined_plot <- (arrow_plot | lollipop_plot) +
  patchwork::plot_layout(
    guides = "collect",
    widths = c(1, 1)
  ) +
  
  # Labs
  plot_annotation(
    title = "Analyzing Regional Sales Performance and Growth Trends",
    subtitle = "An Overview of Sales Changes from Last Year to This Year and Annualized Growth Rate",
    caption = caption_text,
    
    # Theme
    theme = theme(
      plot.title      = element_markdown(
        size          = rel(1.8),   
        hjust         = 0.5,
        family        = "title",
        face          = "bold",
        color         = title_col,
        lineheight    = 1.1,
        margin        = margin(t = 5, b = 5)
      ),
      plot.subtitle   = element_markdown(
        size          = rel(1.1), 
        hjust         = 0.5,
        family        = 'subtitle',
        color         = subtitle_col,
        lineheight    = 1.1, 
        margin        = margin(t = 5, b = 5)
      ),
      plot.caption      = element_markdown(
        size          = rel(.55),
        family        = "caption",
        color         = caption_col,
        lineheight    = 1.1,
        hjust         = 0,
        halign        = 0,
        margin        = margin(t = 5, b = 5)
      ),
    )
    
  )

combined_plot 


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 
 
# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-06-26
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# base        * 4.4.0      2024-04-24 [2] local
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
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
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P marquee     * 0.1.0      2024-05-28 [?] CRAN (R 4.4.0)
# P MetBrewer   * 0.2.0      2022-03-21 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0      2024-04-24 [?] local
# MoMAColors  * 0.0.0.9000 2024-05-02 [1] Github (BlakeRMills/MoMAColors@6f5d75d)
# munsell       0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0      2024-04-24 [?] local
# P patchwork   * 1.2.0      2024-01-08 [?] CRAN (R 4.4.0)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0     2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2      2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0     2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3     2023-11-18 [?] CRAN (R 4.4.0)
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
# P stats       * 4.4.0      2024-04-24 [?] local
# P stringi       1.8.3      2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
# P styler        1.10.3     2024-04-07 [?] CRAN (R 4.4.0)
# P svglite       2.1.3      2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts    * 0.8.9      2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts   1.1.0      2024-05-15 [?] CRAN (R 4.4.0)
# P textshaping   0.4.0      2024-05-24 [?] CRAN (R 4.4.0)
# P tibble      * 3.2.1      2023-03-20 [?] CRAN (R 4.4.0)
# tidyr       * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect    1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse   * 2.0.0      2023-02-22 [?] CRAN (R 4.4.0)
# P timechange    0.3.0      2024-01-18 [?] CRAN (R 4.4.0)
# P tools         4.4.0      2024-04-24 [?] local
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0      2024-04-24 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────
# > 