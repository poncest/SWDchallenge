
# Challenge:  #SWDchallenge 2024 -- September
## Topic:     AUG 2024 | stack it up!
## Author:    Steven Ponce
## Date:      2024-09-01


## 0. DATA SOURCE ----

## Data:      A worldwide database of hydrogen projects (via International Energy Agency) 
## Link:      https://www.iea.org/data-and-statistics/data-product/hydrogen-production-and-infrastructure-projects-database#data-sets


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
  readxl,      # Read Excel Files
  countrycode, # Convert Country Names and Country Codes
  marquee,     # Markdown Parser and Renderer for R Graphics
  ggforce      # Accelerating 'ggplot2'    
)


### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
worldwide_hydrogen_projects <- read_xlsx(
  here::here("2024/09_Sep/data/Hydrogen_production_projects_corrected.xlsx"),
  sheet = "Projects", skip = 3, trim_ws = TRUE
  ) |>
  clean_names() |>
  glimpse()



## 3. EXAMINING THE DATA ----
glimpse(worldwide_hydrogen_projects)
skim(worldwide_hydrogen_projects)
colnames(worldwide_hydrogen_projects)
 


## 4. TIDYDATA ----

plot_data <- worldwide_hydrogen_projects |>
  filter(!is.na(country), !is.na(status)) |>
  count(country, status) |>
  # Lump less common countries into "Other"
  mutate(country = fct_lump(country, n = 10, w = n)) |>
  group_by(country) |>
  mutate(total_projects = sum(n)) |>
  ungroup() |>
  filter(country != "Other") |>
  # Convert ISO3 country codes to country names
  mutate(country = countrycode(country, origin = 'iso3c', destination = 'country.name')) |>
  arrange(desc(total_projects))



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "#3d3d3d"           
subtitle_col <- "#3d3d3d"     
caption_col  <- "gray30"   
text_col     <- colorspace::darken("#8e8a7b" , 0.2)    

# Define colors to highlight Operational projects
highlight_colors <- c(
  "Operational"       = "#E69F00",  # Highlight color
  "Concept"           = "grey80",
  "DEMO"              = "grey70",
  "FID/Construction"  = "grey60",
  "Other"             = "grey50",
  "Decomissioned"     = "grey40",
  "Feasibility study" = "grey30"
)


### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: SEP 2024 &bull; Source: The Hydrogen Production Projects Database<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text   <- str_glue("Hydrogen Projects by Country and Status") 

subtitle_text <- "Focusing on the top 10 countries and highlighting {#E69F00 _**Operational**_} projects"

caption_text <- str_glue("{tt} {li} stevenponce &bull; {mn} @sponce1(graphic.social) &bull; {gh} poncest &bull; #rstats #ggplot2")

### |-  fonts ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf')  
font_add_google("Oswald", regular.wt = 400, family = "title")                 
font_add_google("Quattrocento Sans", regular.wt = 400, family = "subtitle")  
font_add_google("Quattrocento Sans", regular.wt = 400, family = "text")        
font_add_google("Noto Sans", regular.wt = 400,family = "caption")
showtext_auto(enable = TRUE)  

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  
  legend.position       = "top",
  legend.title          = element_text(size = rel(.85), hjust = 0.5),
  legend.text           = element_text(size = rel(0.8)),
  
  plot.margin           = margin(t = 10, r = 15, b = 0, l = 15),       
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), color = text_col, family = 'text', face = 'bold'),
  axis.text             = element_text(size = rel(0.75), color = text_col, family = 'text'),
  axis.line.x           = element_line(color = "#7F7F7F", linewidth = 0.2),
  
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
  
)


### |-  initial plot ----  
p <- plot_data |>
  ggplot(aes(x = reorder(country, -total_projects), y = n, fill = status)) +
  
  # Geoms
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  
  # Add total project labels at the end of each bar
  geom_text(aes(label = total_projects), 
            y = plot_data$total_projects + 5,  
            hjust = 0, color = text_col, size = 3.5) +
  
  # Add labels specifically for Operational projects
  geom_text(aes(label = ifelse(status == "Operational", n, "")), 
            position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  
  # Add country label above Denmark
  geom_text(aes(label = "Country", x = "Denmark", y = 0),
            size = 5.5, color = text_col, family = "text", fontface = "bold", 
            hjust = 1.4, vjust = -1.5) +
  
  
  # Scales
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, 205)) +
  scale_fill_manual(
    values = highlight_colors, 
    guide = guide_legend(nrow = 2)
    ) +
  coord_flip(clip = 'off') +

  # Labs
  labs(
    x = "",
    y = "Number of Projects",
    fill = "Project Status",
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
  ) +
  
  # Theme
  theme(
    
    plot.title    = element_text(
      size        = rel(1.8),
      family      = "title",
      face        = "bold",
      color       = title_col,
      lineheight  = 1.1,
      margin      = margin(t = 5, b = 5)
    ),        
    plot.subtitle = element_marquee(
      size        = rel(1.1),
      family      = 'subtitle',
      color       = subtitle_col,
      lineheight  = 1.4, 
      margin      = margin(t = 5, b = 1)
    ),
    plot.caption  = element_markdown(
      size        = rel(.55), 
      family      = 'caption',
      color       = caption_col,
      lineheight  = 0.6,
      hjust       = 0,
      halign      = 0,
      margin      = margin(t = 10, b = 10)
    )
  )

p

### |-  annotated plot ----  

# label data
label_data <- tibble(
  country = "United Kingdom",
  total_projects = 119
)

p + 
  ggforce::geom_mark_circle(
    data = label_data,
    aes(
      x = country, 
      y = total_projects, 
      label = "", 
      description = "Total H2 Projects per Country",
      ),
    expand = 0.03, 
    label.buffer = unit(-1, "lines"),
    color = "gray70",
    fill = "transparent",
    label.fill = bkg_col,
    label.fontsize = 8,
    label.family = "text",
    con.type = "elbow",
    label.colour = 'gray20', 
    con.colour = 'gray60'
  )

  
# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 
 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-09-01
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────
# ! package     * version    date (UTC) lib source
# V base        * 4.4.1      2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3      2015-07-28 [?] CRAN (R 4.4.0)
# P cachem        1.0.8      2023-05-01 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0      2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0      2016-07-27 [?] CRAN (R 4.4.0)
# P cli           3.6.2      2023-12-11 [?] CRAN (R 4.4.0)
# colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1      2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0      2024-04-24 [?] local
# P countrycode * 1.6.0      2024-03-22 [?] CRAN (R 4.4.1)
# cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
# P curl          5.2.1      2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0      2024-04-24 [?] local
# P devtools      2.4.5      2022-10-11 [?] CRAN (R 4.4.0)
# P digest        0.6.35     2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
# P ellipsis      0.3.2      2021-04-29 [?] CRAN (R 4.4.0)
# P fansi         1.0.6      2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1      2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap       1.1.1      2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
# P fs            1.6.4      2024-04-25 [?] CRAN (R 4.4.0)
# generics      0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
# P ggforce     * 0.4.2      2024-02-19 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
# P ggstream      0.1.0      2021-05-06 [?] CRAN (R 4.4.1)
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
# P htmlwidgets   1.6.4      2023-12-06 [?] CRAN (R 4.4.0)
# P httpuv        1.6.15     2024-03-26 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0      2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8      2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46       2024-04-06 [?] CRAN (R 4.4.0)
# labeling      0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
# P later         1.3.2      2023-12-06 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4      2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3      2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3      2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3      2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.12       2023-12-06 [?] CRAN (R 4.4.0)
# P marquee     * 0.1.0      2024-05-28 [?] CRAN (R 4.4.0)
# P MASS          7.3-60.2   2024-04-24 [?] local
# P memoise       2.0.1      2021-11-26 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0      2024-04-24 [?] local
# P mime          0.12       2021-09-28 [?] CRAN (R 4.4.0)
# P miniUI        0.1.1.1    2018-05-18 [?] CRAN (R 4.4.0)
# munsell       0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1      2019-03-11 [?] CRAN (R 4.4.0)
# P pillar        1.9.0      2023-03-22 [?] CRAN (R 4.4.0)
# P pkgbuild      1.4.4      2024-03-17 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3      2019-09-22 [?] CRAN (R 4.4.0)
# P pkgload       1.3.4      2024-01-16 [?] CRAN (R 4.4.0)
# P polyclip      1.10-6     2023-09-27 [?] CRAN (R 4.4.0)
# P profvis       0.3.8      2023-05-02 [?] CRAN (R 4.4.0)
# P promises      1.3.0      2024-04-05 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2      2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1      2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0      2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12     2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5      2024-01-10 [?] CRAN (R 4.4.0)
# P readxl      * 1.4.3      2023-07-06 [?] CRAN (R 4.4.0)
# P remotes       2.5.0      2024-03-17 [?] CRAN (R 4.4.0)
# renv          1.0.7      2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7      2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.3      2024-01-10 [?] CRAN (R 4.4.0)
# P rprojroot     2.0.4      2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0     2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0      2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2      2021-12-06 [?] CRAN (R 4.4.0)
# P shiny         1.8.1.1    2024-04-02 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7      2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0        2020-06-04 [?] CRAN (R 4.4.0)
# P skimr       * 2.1.5      2022-12-23 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1     2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0      2024-04-24 [?] local
# P stringi       1.8.3      2023-12-11 [?] CRAN (R 4.4.0)
# P stringr     * 1.5.1      2023-11-14 [?] CRAN (R 4.4.0)
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
# P tweenr        2.0.3      2024-02-26 [?] CRAN (R 4.4.0)
# P tzdb          0.4.0      2023-05-12 [?] CRAN (R 4.4.0)
# P urlchecker    1.0.1      2021-11-30 [?] CRAN (R 4.4.0)
# P usethis       2.2.3      2024-02-19 [?] CRAN (R 4.4.0)
# P utf8          1.2.4      2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0      2024-04-24 [?] local
# P vctrs         0.6.5      2023-12-01 [?] CRAN (R 4.4.0)
# P withr         3.0.0      2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43       2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6      2023-12-04 [?] CRAN (R 4.4.0)
# P xtable        1.8-4      2019-04-21 [?] CRAN (R 4.4.0)
# 
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/_CHALLENGES/SWDchallenge/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 
