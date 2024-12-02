
# Challenge:  #SWDchallenge 2024 -- December
## Topic:     DEC 2024 | tell me something good
## Author:    Steven Ponce
## Date:      2024-12-02


## 0. DATA SOURCE ----

## Data:      International Labour Organization 
##            Africa: Lower-middle income - Annual
## Link:      https://rshiny.ilo.org/dataexplorer5/?lang=en&id=X08_A

## Citation:
#' International Labour Organization. Africa: Lower-middle income - Annual
#' [ILOSTAT explorer. Africa: Lower-middle income - Annual (1990 - 2024)]. 
#' Accessed [2024-12-02].


## 1. LOAD PACKAGES & SETUP ----  
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  paletteer,         # Comprehensive Collection of Color Palettes
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  skimr,             # Compact and Flexible Summaries of Data
  camcorder          # Record Your Plot History
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
poverty_rate_data_raw <- read_csv("2024/12_Dec/ILO_X08_A-full-2024-12-02.csv") |> 
  filter(
    indicator.label == "SDG indicator 1.1.1 - Working poverty rate (percentage of employed living below US$2.15 PPP) (%)"
  ) |> 
  clean_names() 



## 3. EXAMINING THE DATA ----
glimpse(poverty_rate_data_raw)
skim(poverty_rate_data_raw)



## 4. TIDYDATA ----
poverty_rate_clean <- poverty_rate_data_raw |> 
  select(-obs_status_label, -note_classif_label, -note_indicator_label, -note_source_label, -classif2_label) |> 
  rename(
    year = time,
    pct = obs_value
  ) |> 
  mutate(
    sex_label = str_remove(sex_label, pattern = "Sex: "),
    pct = pct / 100,
    age_bin = case_when(
      classif1_label == "Age (Youth, adults): 15+" ~ "Ages 15 and Older",
      classif1_label == "Age (Youth, adults): 15-24" ~ "Youth: Ages 15-24",
      classif1_label == "Age (Youth, adults): 25+" ~ "Adults: Ages 25 and Older",
      TRUE ~ classif1_label
    ),
    age_bin = factor(age_bin, levels = c("Ages 15 and Older", "Youth: Ages 15-24", "Adults: Ages 25 and Older"))
  ) |> 
  filter(year <= 2019) |> 
  pivot_wider(names_from = sex_label, values_from = pct)



## 5. VISUALIZATION ---- 

### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("nbapalettes::mavericks_retro")

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Dec 2024 &bull; Source: Source: Source: ILOSTAT<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
women  <- str_glue("<span style='font-size:16pt; color:{ col_palette[1] }'>**Women**</span>")
men    <- str_glue("<span style='font-size:16pt; color:{ col_palette[2] }'>**Men**</span>")
total  <- str_glue("<span style='font-size:16pt; color:{ col_palette[3] }'>**Total**</span>")

title_text   <- str_glue("Progress in Reducing Working Poverty Rates in Africa (2000-2019)") 

subtitle_text <- str_glue("Poverty rates for both {women} and {men} have steadily declined across all age groups in Africa\\'s<br>
                           lower-middle-income countries. Adults and youth have shown different paces of reduction in poverty,<br>
                           with all populations living below $2.15/day PPP. The dashed line represents the { total } (combined average)<br><br>
                          <span style='font-size:15.4pt;'><strong>Percentage of Workers in Poverty</strong></span>")

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
  legend.position       = "plot",
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "#252525", linewidth = .3),
  axis.ticks.x          = element_line(color = text_col),  
  axis.title            = element_text(face = "bold"),
  panel.grid.minor      = element_blank(),
  panel.grid.major      = element_blank(),
  panel.grid.major.y    = element_line(color = "grey85", linewidth = .4),
  strip.background      = element_rect(fill = "#f0f0f0", color = NA),
  strip.text            = element_textbox(size = rel(1),  
                                          face = 'bold',
                                          color = text_col,
                                          hjust = 0.5,
                                          halign = 0.5,
                                          r = unit(5, "pt"),
                                          width = unit(5.5, "npc"),
                                          padding = margin(3, 0, 3, 0),
                                          margin = margin(3, 3, 3, 3),
                                          fill = "transparent"
  ),
  panel.spacing         = unit(1.5, 'lines'),
)  

### |- initial plot ----  
poverty_rate_clean |>
  ggplot(aes(x = year)) +
  # Fill the gap between Male and Female
  geom_ribbon(aes(ymin = Male, ymax = Female), fill = "gray75", alpha = 0.3) +
  # Geoms for Male and Female
  geom_line(aes(y = Female, color = "Female"), linewidth = 1) +
  geom_line(aes(y = Male, color = "Male"), linewidth = 1) +
  # Geom for Total (dashed line)
  geom_line(aes(y = Total, linetype = "Total"), color = col_palette[3], linewidth = 0.55, alpha = 0.6) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(2000, 2020, by = 10),
    limits = c(2000, 2020)
  ) +
  scale_y_continuous(
    breaks = seq(0, .5, by = .1),
    limits = c(.1, .50),
    label = percent_format()
  ) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = col_palette) +
  scale_linetype_manual(values = c("dashed")) +
  
  # Labs
  labs(
    x = "Year",
    y = NULL,
    color = "Gender",
    linetype = "Total",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) +
  
  # Facet
  facet_wrap(~ age_bin, nrow = 1) +
  
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.7),
      family = "title",
      face = "bold",
      color = title_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.95),
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

# ─ Session info ────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-12-02
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit           4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64         4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder   * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
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
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P paletteer   * 1.6.0    2024-01-21 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P prismatic     1.1.2    2024-04-10 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P rematch2      2.1.2    2020-05-01 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.4    2024-06-04 [?] CRAN (R 4.4.1)
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
# ───────────────────────────────────────────────────────────────────
# >