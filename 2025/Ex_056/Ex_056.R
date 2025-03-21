
## Challenge: #SWDchallenge 2025-- March Exercise
## Topic:     Exercises | resist the temptation to show all the data
## Data:      Let's Practice! Exercise 5.6
## Author:    Steven Ponce
## Date:      2025-03-19


## 0. DATA SOURCE ----
#' 
#' This exercise is based on Let's Practice! Exercise 5.6 
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1P6G8z8Wcj4GtSkxkhBmaSS3AAeXPgJeZ/edit?gid=1336591892#gid=1336591892
#' 

## Updated to incorporate feedback from Antti Rask


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
  camcorder,         # Record Your Plot History,
  ggrepel,           # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
  patchwork          # The Composer of Plots # The Composer of Plots # The Composer of Plots
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 10,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- read_csv("2025/Ex_056/swdexercise056.csv") |>
  clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----
tidy_data <- raw_data |>
  rename(month = column1, year = column2) |>
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), 
                   format = "%Y-%b-%d")
  ) |>
  select(date, everything(), -month, -year) |>
  pivot_longer(
    cols = -date, 
    names_to = "department", 
    values_to = "growth"
  ) |> 
  mutate(
    year_month = format(date, "%b %Y"),
    quarter = paste0("Q", quarter(date)),
    year_quarter = paste(year(date), quarter, sep = "-")
  )

# Compute summary statistics for each department
summary_stats <- tidy_data |>
  group_by(department) |>
  summarise(
    mean_growth = mean(growth, na.rm = TRUE),
    min_growth = min(growth, na.rm = TRUE),
    max_growth = max(growth, na.rm = TRUE),
    sd_growth = sd(growth, na.rm = TRUE),
    range_growth = max_growth - min_growth,
    cv_growth = sd_growth / abs(mean_growth),  
    .groups = 'drop'
  ) |>
  arrange(desc(sd_growth))

# Summary statistics 
stats_table <- summary_stats |>
  select(department, mean_growth, sd_growth, min_growth, max_growth, range_growth) |>
  mutate(across(where(is.numeric), ~ round(., 2)))

# Identify the most volatile departments (using SD as criteria)
volatile_departments <- summary_stats |> 
  slice_max(order_by = sd_growth, n = 3) |> 
  pull(department)

# Prepare the data for plotting
highlight_data <- tidy_data |> 
  filter(department %in% volatile_departments)


## 5. VISUALIZATION ---- 
# Get base colors with custom palette

# Get the departments in the correct order (by volatility)
volatile_departments <- summary_stats |> 
  slice_max(order_by = sd_growth, n = 3) |> 
  pull(department)

# Create a categorical palette for three volatile departments
cat_colors <- c(
  "grocery" = "#0F62FE",   
  "hardware" = "#FF7EB6",  
  "toys" = "#6929C4"        
)

# Get the rest of the departments
other_departments <- setdiff(unique(tidy_data$department), volatile_departments)

# Create a palette for all departments (both highlighted and non-highlighted)
all_dept_colors <- c(cat_colors, 
                     setNames(rep("gray80", length(other_departments)), 
                              other_departments))

# Colors theme function
colors <- get_theme_colors(
  palette = all_dept_colors
)

### |-  titles and caption ----
title_text <- str_glue("Retail Department Performance Analysis: Growth and Volatility")
subtitle_text <- str_glue("Understanding stability patterns and growth trajectories across retail categories\n(Jan 2024 - Mar 2025)")

# Format the names with colors for the descriptive title
grocery_color <- cat_colors["grocery"]
hardware_color <- cat_colors["hardware"]
toys_color <- cat_colors["toys"]

# P1 chart title
rich_title <- glue::glue(
  "<span style='color:{grocery_color}'>Grocery</span>, ",
  "<span style='color:{hardware_color}'>Hardware</span>, and ",
  "<span style='color:{toys_color}'>Toys</span> are the three most volatile departments"
)

# P2 chart title
rich_title_p2 <- glue::glue(
  "<span style='color:{grocery_color}'>Grocery</span> shows highest volatility despite below-average growth"
)

# Create caption
caption_text <- create_swd_exe_caption(
  year = 2025,
  month = "Mar",
  source_text =  "Let's Practice! Exercise 5.6"
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
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
  )
)

# Set theme
theme_set(weekly_theme)

# |- plot ----

# P1. Line Chart ----
p1 <- ggplot() +
  # Geoms
  geom_hline(
    yintercept = 0, color = "gray30", alpha = 0.5, linewidth = 0.5
  ) +
  geom_line(data = tidy_data, 
            aes(x = date, y = growth, group = department), 
            color = "gray", linewidth = 0.2, alpha = 0.8
  ) +
  geom_line(data = highlight_data,           # Highlighted volatile departments
            aes(x = date, y = growth, color = department), 
            linewidth = 1.2
  ) +
  geom_point(data = highlight_data,
             aes(x = date, y = growth, color = department),
             size = 2
  ) +
  geom_text_repel(
    data = highlight_data |> 
      group_by(department) |> 
      filter(date == max(date)),
    aes(x = date, y = growth, label = str_to_title(department), color = department),
    hjust = -0.2, size = 4, fontface = "bold",
    direction = "y", segment.color = "gray50",
    box.padding = 0.5, point.padding = 0.3
  ) +
  
  # Scales
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.1))  
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(-3, 7, by = 1)
  ) +
  scale_color_manual(values = cat_colors) +  
  # Labs
  labs(
    title = rich_title,
    subtitle = "Department Year-over-Year Growth Trends",
    y = NULL,
    x = NULL,
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_line(color = "gray90"),
    plot.title = element_markdown(
      size = rel(1.1), 
      family = fonts$title,
      face = "bold",
      margin = margin(b = 10)
    )
  )

# P2. Scatter Plot ----
p2 <- ggplot(summary_stats,
             aes(x = mean_growth, y = sd_growth, label = department)) +
  # Geoms
  geom_point(
    aes(color = department, size = range_growth),
    alpha = 0.8
  ) +
  geom_text_repel(
    size = 3.5,
    box.padding = 0.5,
    max.overlaps = 15,
    segment.color = "gray70"
  ) +
  # Scales
  scale_color_manual(
    values = all_dept_colors  # applies the same colors to all departments
  ) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = rich_title_p2,
    subtitle = "Department Performance: Comparing Growth and Volatility",
    x = "Mean Growth Rate (%)",
    y = NULL   
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.1), 
      family = fonts$title,
      face = "bold",
      margin = margin(b = 10)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey70", linewidth = 0.1),
  )

# Combined Plot -----
combined_plot <- (p1 / p2) +
  plot_layout(heights = c(1, 1))   

combined_plot <- combined_plot +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text( 
        size = rel(1.4),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(0.95),  
        family = fonts$subtitle,
        color = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size   = rel(0.65),
        family = fonts$caption,
        color  = colors$caption,
        hjust  = 0.5,
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
    ))

combined_plot


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-19
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────
# ! package     * version  date (UTC) lib source
# P annotater     0.2.3    2024-01-26 [?] CRAN (R 4.4.0)
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
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
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# ggrepel     * 0.9.6    2024-09-07 [1] CRAN (R 4.4.1)
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
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P patchwork   * 1.2.0    2024-01-08 [?] CRAN (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
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
# P utf8          1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom         1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# xfun          0.50     2025-01-07 [1] CRAN (R 4.4.2)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────
# > 
