
## Challenge: #SWDchallenge 2025 -- Juy Exercise
## Topic:     visualize a multi-dimensional data table
## Author:    Steven Ponce
## Date:      2025-07-09


## 0. DATA SOURCE ----
#' 
#' The data can be "download" at: 
#' https://community.storytellingwithdata.com/exercises/visualize-a-multi-dimensional-data-table
#' 

## Business Context:
#' STEP 1: Brainstorm how you could visualize this data. Create a list of various 
#' graphs that could effectively highlight relationships or patterns. Make 
#' assumptions as needed and decide how much or how little of this data is 
#' important to show in a single graph.

#' STEP 2: From the list you’ve made, create at least three different views. 
#' (These visuals should be rough drafts, rather than polished, final versions.) 
#' You may sketch these by hand or use your preferred graphing tool. 

#' STEP 3: Consider each view and evaluate what pattern or insight it shows. 
#' How does each one help (or hinder) understanding? Determine which visual(s) 
#' you would share with the leadership team and why. (Note: there is no single “right” answer.)


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  RColorBrewer,      # ColorBrewer Palettes 
  ggrepel            # Automatically Position Non-Overlapping Text Labels with 'ggplot2'      
) 

### |- figure size ---- 
camcorder::gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320)


## 2. READ IN THE DATA ----
raw_data <- read_csv("2025/Ex_July/Employee Engagement Survey Results.csv") |>
  janitor::clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY DATA ----

# Clean and prepare the data
clean_data <- raw_data |>
  mutate(
    salary_numeric = as.numeric(gsub("[\\$,]", "", average_salary)),
    tenure_factor = factor(tenure_range,
      levels = c("<1 year", "1-5 years", "5+ years"),
      ordered = TRUE
    ),
    dept_tenure = paste(department, tenure_range, sep = " - ")
  )

# Calculate correlations
engagement_salary_cor <- cor(clean_data$average_engagement, clean_data$salary_numeric)
engagement_performance_cor <- cor(clean_data$average_engagement, clean_data$average_performance_rating)
salary_performance_cor <- cor(clean_data$salary_numeric, clean_data$average_performance_rating)

cat("Engagement vs Salary: r =", round(engagement_salary_cor, 3), "\n")
cat("Engagement vs Performance: r =", round(engagement_performance_cor, 3), "\n")
cat("Salary vs Performance: r =", round(salary_performance_cor, 3), "\n\n")


## 5. VISUALIZATIONS ----
#  HEATMAP
p1 <- clean_data |>
  mutate(
    engagement_category = case_when(
      average_engagement >= 80 ~ "High (80+)",
      average_engagement >= 70 ~ "Medium (70-79)",
      TRUE ~ "Low (60-69)"
    ),
    engagement_category = factor(engagement_category,
      levels = c("Low (60-69)", "Medium (70-79)", "High (80+)")
    )
  ) |>
  ggplot(aes(x = tenure_factor, y = department, fill = engagement_category)) +
  # Geoms
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = average_engagement), color = "white", size = 6, fontface = "bold") +
  # Scales
  scale_fill_manual(
    name = "Engagement Level",
    values = c(
      "Low (60-69)" = "#ca0020",
      "Medium (70-79)" = "#f4a582",
      "High (80+)" = "#0571b0"
    )
  ) +
  # Labs
  labs(
    title = "Employee Engagement by Department and Tenure",
    subtitle = "Blue = High performance, Red = Areas needing attention",
    x = "Tenure Range",
    y = "Department"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    panel.background = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid = element_blank()
  )

# ENGAGEMENT vs SALARY
p2 <- clean_data |>
  ggplot(aes(x = salary_numeric, y = average_engagement)) +
  # Geoms
  geom_point(aes(color = department, shape = tenure_factor), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray20", linetype = "dashed", linewidth = 0.5) +
  geom_text_repel(aes(label = paste(department, tenure_range, sep = "\n")),
    size = 3, point.padding = 0.5
  ) +
  # Scales
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +
  scale_color_brewer(name = "Department", type = "qual", palette = "Set1") +
  scale_shape_manual(
    name = "Tenure",
    values = c("<1 year" = 16, "1-5 years" = 17, "5+ years" = 15)
  ) +
  # Labs
  labs(
    title = paste("Engagement vs Salary Relationship (r =", round(engagement_salary_cor, 2), ")"),
    subtitle = "Higher salaries are associated with higher engagement",
    x = "Average Salary",
    y = "Average Engagement Score"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    panel.background = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    legend.position = "plot",
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold")
  )

# ENGAGEMENT vs PERFORMANCE
p3 <- clean_data |>
  ggplot(aes(x = average_engagement, y = average_performance_rating)) +
  # Geoms
  geom_point(aes(color = department, shape = tenure_factor), size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray20", linetype = "dashed", linewidth = 0.5) +
  geom_text_repel(aes(label = paste(department, tenure_range, sep = "\n")),
    size = 3, point.padding = 0.5
  ) +
  # Scales
  scale_color_brewer(name = "Department", type = "qual", palette = "Set1") +
  scale_shape_manual(
    name = "Tenure",
    values = c("<1 year" = 16, "1-5 years" = 17, "5+ years" = 15)
  ) +
  # Labs
  labs(
    title = paste("Engagement vs Performance Relationship (r =", round(engagement_performance_cor, 2), ")"),
    subtitle = "Strong correlation: Higher engagement predicts better performance",
    x = "Average Engagement Score",
    y = "Average Performance Rating"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    panel.background = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    legend.position = "plot",
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold")
  )

# QUADRANT ANALYSIS

# Calculate medians for quadrant lines
salary_median <- median(clean_data$salary_numeric)
engagement_median <- median(clean_data$average_engagement)

p4 <- clean_data |>
  # Create quadrant categories
  mutate(
    quadrant = case_when(
      salary_numeric >= salary_median & average_engagement >= engagement_median ~ "High Salary,\nHigh Engagement",
      salary_numeric >= salary_median & average_engagement < engagement_median ~ "High Salary,\nLow Engagement",
      salary_numeric < salary_median & average_engagement >= engagement_median ~ "Low Salary,\nHigh Engagement",
      TRUE ~ "Low Salary,\nLow Engagement"
    ),
    quadrant = factor(quadrant, levels = c(
      "High Salary,\nHigh Engagement",
      "High Salary,\nLow Engagement",
      "Low Salary,\nHigh Engagement",
      "Low Salary,\nLow Engagement"
    ))
  ) |>
  ggplot(aes(x = salary_numeric, y = average_engagement)) +
  # Annotate
  annotate("rect",
    xmin = salary_median, xmax = Inf, ymin = engagement_median, ymax = Inf,
    alpha = 0.1, fill = "green"
  ) + # Top-right: ideal
  annotate("rect",
    xmin = salary_median, xmax = Inf, ymin = -Inf, ymax = engagement_median,
    alpha = 0.1, fill = "orange"
  ) + # Bottom-right: high cost, low engagement
  annotate("rect",
    xmin = -Inf, xmax = salary_median, ymin = engagement_median, ymax = Inf,
    alpha = 0.1, fill = "blue"
  ) + # Top-left: good engagement despite low salary
  annotate("rect",
    xmin = -Inf, xmax = salary_median, ymin = -Inf, ymax = engagement_median,
    alpha = 0.1, fill = "red"
  ) + # Bottom-left: problematic
  annotate("text",
    x = salary_median + 25000, y = engagement_median + 7,
    label = "IDEAL\nHigh Salary & Engagement", size = 4, fontface = "bold",
    hjust = 0.5, color = "darkgreen"
  ) +
  annotate("text",
    x = salary_median + 25000, y = engagement_median - 7,
    label = "EXPENSIVE\nHigh Salary, Low Engagement", size = 4, fontface = "bold",
    hjust = 0.5, color = "darkorange"
  ) +
  annotate("text",
    x = salary_median - 25000, y = engagement_median + 7,
    label = "OPPORTUNITY\nHigh Engagement, Low Salary", size = 4, fontface = "bold",
    hjust = 0.5, color = "darkblue"
  ) +
  annotate("text",
    x = salary_median - 25000, y = engagement_median - 7,
    label = "PROBLEMATIC\nLow Salary & Engagement", size = 4, fontface = "bold",
    hjust = 0.5, color = "darkred"
  ) +
  # Geoms
  geom_vline(xintercept = salary_median, linetype = "dashed", color = "gray20", linewidth = .5) +
  geom_hline(yintercept = engagement_median, linetype = "dashed", color = "gray20", linewidth = .5) +
  # Add points
  geom_point(aes(color = department, size = average_performance_rating), alpha = 0.8) +
  geom_text_repel(aes(label = paste(department, tenure_range, sep = "\n")),
    size = 3, point.padding = 0.5, max.overlaps = 15
  ) +
  # Scales
  scale_x_continuous(
    labels = scales::dollar_format(scale = 1e-3, suffix = "K"),
    limits = c(40000, 145000)
  ) +
  scale_color_brewer(name = "Department", type = "qual", palette = "Set1") +
  scale_size_continuous(name = "Performance\nRating", range = c(3, 10)) +
  # Labs
  labs(
    title = "Salary vs Engagement Quadrant Analysis",
    subtitle = paste("Quadrant lines at median salary ($", round(salary_median / 1000, 0), "K) and median engagement (", engagement_median, ")", sep = ""),
    x = "Average Salary",
    y = "Average Engagement Score"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    panel.background = element_rect(fill = "#FDFDFD", color = "#FDFDFD"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank()
  )


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-07-10
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P bit            4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64          4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli            3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# P crayon         1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
# dplyr        * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# farver         2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# ggrepel      * 0.9.6    2024-09-07 [1] CRAN (R 4.4.1)
# P ggtext       * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski         1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue         * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# P gridtext       0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable         0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here           1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms            1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P janitor        2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite       1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle      1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate    * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick         2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr       2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P nlme           3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman         0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# pillar         1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig      2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr        * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache        0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3    1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo           1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils        2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6             2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg           1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# RColorBrewer * 1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# P Rcpp           1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr        * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv           1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# rlang          1.1.5    2025-01-17 [1] CRAN (R 4.4.2)
# P rprojroot      2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi     0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg           2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext     * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb   * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P snakecase      0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P splines        4.4.0    2024-04-24 [?] local
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
# P stringr      * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P styler         1.10.3   2024-04-07 [?] CRAN (R 4.4.0)
# P svglite        2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts     * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts    1.1.0    2024-05-15 [?] CRAN (R 4.4.0)
# P textshaping    0.4.0    2024-05-24 [?] CRAN (R 4.4.0)
# P tibble       * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidyverse    * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange     0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# P tzdb           0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# P vctrs          0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom          1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# P xml2           1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────
# > 