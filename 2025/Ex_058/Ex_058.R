
## Challenge: #SWDchallenge 2025-- May Exercise
## Topic:     Exercises | which chart shows it best?
## Data:      Let's Practice! Exercise 5.8
## Author:    Steven Ponce
## Date:      2025-05-23


## 0. DATA SOURCE ----
#' 
#' This exercise is based on Let's Practice! Exercise 5.8 
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/125uh8nYavGu-tYc3tlulDIchmoVZQqBk/edit?gid=1444625075#gid=1444625075
#' 


## Business Context:
#' You're helping a concierge service evaluate their client contact program. They need to:

#' Show which departments improved at contacting clients quarterly
#' Identify underperforming areas that need accountability
#' Recognize departments that should be celebrated for improvement
#' Drive actionable discussions about performance changes
#' 


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  patchwork          # The Composer of Plots 
) 

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- readxl::read_excel("2025/Ex_058/swdexercise058 DATA.xlsx", 
                              sheet = "MAIN", range = "B4:D9",trim_ws = TRUE ) |>
  janitor::clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----
tidy_data <- raw_data |>
  filter(category != "Total") |>
  pivot_longer(cols = c(last_quarter, this_quarter), 
               names_to = "period", 
               values_to = "contact_rate") |>
  mutate(
    period = case_when(
      period == "last_quarter" ~ "Before Program",
      period == "this_quarter" ~ "After Program"
    ),
    period = factor(period, levels = c("Before Program", "After Program"))
  ) |> 
  mutate(
    category_short = case_when(
      category == "Elite access & bespoke requests" ~ "Elite Access",
      category == "Travel & experiences" ~ "Travel & Experiences", 
      category == "Executive services" ~ "Executive Services",
      category == "Lifestyle services" ~ "Lifestyle Services",
      TRUE ~ category
    )
  )

change_data <- raw_data |>  
  filter(category != "Total") |>
  mutate(
    change = this_quarter - last_quarter,
    # Shorter category names for better display
    category_short = case_when(
      category == "Elite access & bespoke requests" ~ "Elite Access",
      category == "Travel & experiences" ~ "Travel & Experiences", 
      category == "Executive services" ~ "Executive Services",
      category == "Lifestyle services" ~ "Lifestyle Services",
      TRUE ~ category
    ),
    strategic_color = case_when(
      change >= 0.15 ~ "exceptional",     
      change >= 0.05 ~ "strong",        
      change >= -0.01 ~ "stable",       
      TRUE ~ "concerning"               
    ),
    action_needed = case_when(
      change >= 0.15 ~ "Recognize & Scale",
      change >= 0.05 ~ "Reinforce Success", 
      change >= -0.01 ~ "Monitor Closely",
      TRUE ~ "Immediate Intervention"
    )
  )


## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = c(
    "exceptional"= "#1f77b4", "strong"= "#969696", "stable" = "#bdbdbd", "concerning" = "#d62728",
    "Executive services"= "#1f77b4", "Travel & experiences"= "#969696", 
    "Elite access & bespoke requests" = "#bdbdbd", "Lifestyle services" = "#d62728"
  )
)

### |-  titles and caption ----
title_text <- str_glue("Client Contact Program: Uneven Success Demands Strategic Response")
subtitle_text <- str_glue("Overall program succeeded, but stark performance gaps require immediate\nresource reallocation")

# Create caption
caption_text <- create_swd_exe_caption(
  year = 2025,
  month = "May",
  source_text =  "Let's Practice! Exercise 5.8"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Text styling 
    plot.title = element_text(face = "bold", family = fonts$title, color = colors$title, size = rel(1.14), margin = margin(b = 10)),
    plot.subtitle = element_text(family = fonts$subtitle, color = colors$text, size = rel(0.78), margin = margin(b = 20)),
    
    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.8)),
    axis.text = element_text(color = colors$text, size = rel(0.7)),
    
    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.1),
    
    # Legend elements
    legend.position = "plot",
    legend.title = element_text(family = fonts$text, size = rel(0.8)),
    legend.text = element_text(family = fonts$text, size = rel(0.7)),
    
    # Plot margins 
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15),
  )
)

# Set theme
theme_set(weekly_theme)

# |- plot ----

# P1. Diverging Chart ----
p1 <- ggplot(change_data, aes(
  x = reorder(category_short, change),
  y = change, fill = strategic_color
)) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5, alpha = 0.8) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(ifelse(change > 0, "+", ""), round(change * 100, 1), "pp")),
    hjust = ifelse(change_data$change > 0, -0.1, 1.1),
    size = 4.2, fontface = "bold", color = "black"
  ) +
  # Scales
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x * 100, "pp"),
    breaks = seq(-0.1, 0.20, 0.05),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  scale_fill_manual(values = colors$palette) +
  coord_flip() +
  # Labs
  labs(
    title = "Executive Services Soars (+20pp), Lifestyle Services Stumbles (-3pp)",
    subtitle = "Performance gaps this wide demand immediate strategic intervention",
    x = "",
    y = "Change in Contact Rate"
  ) +
  # Theme
  theme(
    plot.title = element_text(size = rel(1), face = "bold", color = colors$title, family = fonts$title),
    plot.subtitle = element_text(size = rel(0.71), color = colors$subtitle, family = fonts$subtitle),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )

# P2. Slope Chart ----
p2 <- ggplot(tidy_data, aes(x = period, y = contact_rate, group = category_short, color = category)) +
  # Geoms
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "gray40", alpha = 0.6, size = 0.5) +
  geom_line(
    aes(size = ifelse(category_short == "Executive Services", 3,
      ifelse(category_short == "Lifestyle Services", 2.0, 1.0)
    )),
    alpha = 0.9
  ) +
  geom_point(aes(size = ifelse(category_short %in% c("Executive Services", "Lifestyle Services"), 3, 2))) +
  geom_text(
    data = tidy_data |>
      filter(
        period == "After Program",
        category_short %in% c("Executive Services", "Lifestyle Services")
      ),
    aes(label = category_short),
    hjust = -0.1, vjust = 0.5, size = 3.2, color = "black"
  ) +
  # Annotate
  annotate("text",
    x = 1.5, y = 0.92, label = "90% Target",
    size = 3, color = "gray40", fontface = "italic"
  ) +
  # Scales
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.65, 1.0),
    breaks = seq(0.70, 1.0, 0.10)
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.35))) +
  scale_color_manual(values = colors$palette) +
  scale_size_identity() +
  labs(
    title = "The Journey: Dramatic Trajectories Reveal Strategic Opportunities",
    subtitle = "Steepest slopes indicate departments to celebrate and investigate",
    x = "",
    y = "Client Contact Rate"
  ) +
  # Theme
  theme(
    legend.position = "none",
    plot.title = element_text(size = rel(1), face = "bold", color = colors$title, family = fonts$title),
    plot.subtitle = element_text(size = rel(0.71), color = colors$subtitle, family = fonts$subtitle),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray60", linewidth = 0.25),
  )

# Combined Plot -----
combined_plot <- (p1 / p2) +
  plot_layout(heights = c(1, 1.1))

combined_plot <- combined_plot +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.25),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(0.9),
        family = fonts$subtitle,
        color = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size   = rel(0.6),
        family = fonts$caption,
        color  = colors$caption,
        hjust  = 0.5,
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
    )
  )

combined_plot


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-23
# rstudio  2025.05.0+496 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl          5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
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
# P janitor       2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P patchwork   * 1.2.0    2024-01-08 [?] CRAN (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl        1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P rematch       2.0.0    2023-08-30 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext    * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb  * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase     0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
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
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# xfun          0.50     2025-01-07 [1] CRAN (R 4.4.2)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────
# > 