
# Challenge:  #SWDchallenge 2024 -- October
## Topic:     OCT 2024 | trick (or treat) your tool
## Author:    Steven Ponce
## Date:      2024-10-02


## 0. DATA SOURCE ----

## Data:     Astronaut database (tt 2020 wk 29)
## Link:     https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-07-14



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
  ggalluvial,  # Alluvial Plots in 'ggplot2'
  patchwork,   # The Composer of Plots
  gghighlight  # Highlight Lines and Points in 'ggplot2'
)

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 12,
  height = 8,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
astronaut_db <- read_csv(
  here::here("2024/10_Oct/data/astronauts.csv")
  ) |>
  clean_names() |>
  glimpse()



## 3. EXAMINING THE DATA ----
glimpse(astronaut_db)
skim(astronaut_db)
colnames(astronaut_db)
 


## 4. TIDYDATA ----

# tidy
astronaut_db_clean <- astronaut_db |>
  mutate(
    nationality_grouped = case_when(
      nationality == "U.S." ~ "US",
      nationality == "U.S.S.R/Russia" ~ "U.S.S.R/Russia",
      TRUE ~ "Others"
    ),
    mission_type = str_to_title(military_civilian),
    gender = str_to_title(sex), 
    occupation = str_to_title(occupation),
    occupation = case_when(
      occupation == "Flight Engineer" ~ "Flight Eng.",
      occupation == "Psp" ~ "PSP",
      occupation == "Msp" ~ "MSP",
      occupation == "Spaceflight Participant" ~ "Spacefl. Part.",
      occupation == "Other (Journalist)" ~ "Journalist",
      occupation %in% c("Other (Space Tourist)", "Space Tourist") ~ "Space Tourist (Other)",
      TRUE ~ as.character(occupation)
    )
  ) |>
  select(year_of_mission, nationality_grouped, mission_type, gender, occupation) |>
  filter(!is.na(year_of_mission))



# 5. VISUALIZATION ---- 

### |- plot aesthetics ---- 
bkg_col      <- colorspace::lighten('#f7f5e9', 0.05)    
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("nbapalettes::cavaliers")[c(1,2,3)] 

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: OCT 2024 &bull; Source: Astronaut database (tidytuesday 2020 wk 29)<br>")  
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")  
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
mn <- str_glue("<span style='font-family:fa6-brands'>&#xf4f6;</span>")

# text
title_text   <- str_glue("Astronaut Mission Trends and Career Paths: A Journey from Nationality to Occupation") 

subtitle_text <- str_glue("An exploration of astronaut missions over time, highlighting key periods of activity 
                          across different nationalities, and the flow of <br>
                          astronauts through various career stages from nationality, mission type, gender, to occupation.")

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
  legend.position       = 'plot',
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "gray40", linewidth = .15),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray10'),
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_blank(),
)


### |-  Plot 1 ----  

#  Line Chart 
mission_summary <- astronaut_db_clean |>
  group_by(year_of_mission, nationality_grouped) |>
  summarise(num_missions = n(), .groups = "drop")

# Annotations df 
annotations <- tibble(
  year = c(1969, 1981),  
  label = c(
    "Apollo 11 Moon Landing", 
    "First Space Shuttle Launch"
  ),
  nationality_grouped = c("US", "US"),  
  y_positions = c(43, 32)  
)

p1 <- mission_summary |>
  ggplot(aes(x = year_of_mission, y = num_missions, 
             color = nationality_grouped, group = nationality_grouped)) +
  
  # Geoms
  geom_line(linewidth = 1) + 
  geom_point(size = 1.5) + 
  gghighlight::gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(linewidth = 0.5, size = 0.8)
  ) +
  
  # Annotations
  geom_vline(data = annotations, aes(xintercept = year), 
             linetype = "dashed", color = "darkred", linewidth = 0.2) +
  geom_text(data = annotations, aes(x = year, y = y_positions, label = label),
            size = 2.5, color = "grey30", hjust = 0, nudge_x = 0.8) +  
  
  # Scales
  scale_x_continuous() +
  scale_y_continuous(limits = c(0, max(mission_summary$num_missions) + 5)) +  
  scale_color_manual(values = col_palette) + 
  coord_cartesian(clip = 'off') +
  
  # Labs
  labs(
    title = "Number of Missions Over Time",
    x = "Year of Mission",
    y = "Number of Missions",
    color = "Nationality"
  ) +
  
  # Facet
  facet_wrap(~ nationality_grouped, ncol = 1) +
  
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1),
      hjust = 0.5,
      family = "title",
      color = title_col,
      face = "bold",
      lineheight = 0.85,
      margin = margin(t = 5, b = 5)
    )
  )

### |-  Plot 2 ----  

#  Alluvial Plot 
p2 <- astronaut_db_clean |>
  count(nationality_grouped, mission_type, gender, occupation) |>
  ggplot(aes(
    axis1 = nationality_grouped, axis2 = mission_type, axis3 = gender, axis4 = occupation,
    y = n)
  ) +
  
  # Geoms
  geom_alluvium(aes(fill = nationality_grouped), alpha = 0.6) +  
  geom_stratum(width = 1/4, fill = bkg_col, linewidth = 0.4, colour = 'gray30') +  
  
  ggrepel::geom_text_repel(
    aes(label = after_stat(stratum), family = "text"),
    stat = "stratum", size = 3, direction = "y", nudge_x = -0.3, nudge_y = 1,
    color = "gray10", segment.color = "grey50"
  ) +
  
  # Scales
  scale_x_discrete(limits = c("Nationality", "Mission Type", "Gender", "Occupation"), 
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = col_palette) +  
  
  # Labs
  labs(
    title = "Flow of Astronauts from Nationality to Occupation",
    x = "",
    y = "Count"
  ) +
  
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1),
      hjust = 0.5,
      family = "title",
      color = title_col,
      face = "bold",
      lineheight = 0.85,
      margin = margin(t = 5, b = 5)
    )
  )

### |-  Combine the plots using patchwork ----  

combined_plot <- (p1 | p2) +
  patchwork::plot_layout(
    ncol = 2,
    widths = c(0.75, 1.25), 
    guides = "collect"
  ) +
  
  # Labs
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    
    # Theme 
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.6),
        family = "title",
        face = "bold",
        color = title_col,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1),
        family = "subtitle",
        color = subtitle_col,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(
        size = rel(0.65),
        family = "caption",
        color = caption_col,
        lineheight = 1.1,
        hjust = 0.5,
        halign = 1,
        margin = margin(t = 5, b = 5)
      )
    )
  )


# Show the combined plot
combined_plot


  
# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 
 
# ─ Session info ─────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-10-01
# rstudio  2024.09.0+375 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
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
# ggalluvial  * 0.12.5   2023-02-22 [1] CRAN (R 4.4.1)
# gghighlight * 0.4.1    2023-12-16 [1] CRAN (R 4.4.1)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# ggrepel       0.9.6    2024-09-07 [1] CRAN (R 4.4.1)
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
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P paletteer     1.6.0    2024-01-21 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P patchwork   * 1.2.0    2024-01-08 [?] CRAN (R 4.4.0)
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P prismatic     1.1.2    2024-04-10 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P rematch2      2.1.2    2020-05-01 [?] CRAN (R 4.4.0)
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
# ────────────────────────────────────────────────────────────
# > 