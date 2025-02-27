
## Challenge: #SWDchallenge 2025-- Februart Exercise
## Topic:     Exercises | go crazy or keep it simple
## Data:      Let's Practice! Exercise 5.5
## Author:    Steven Ponce
## Date:      2025-02-28


## 0. DATA SOURCE ----
#' 
#' This exercise is based on Let's Practice! Exercise 5.5. 
#' 
#' The data can be download at: 
#' https://docs.google.com/spreadsheets/d/1ZMYcLPHSb1UyymI3b-_o2PFL7ePDgM89/edit?gid=1857542315#gid=1857542315
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
  camcorder,         # Record Your Plot History,
  paletteer,         # Comprehensive Collection of Color Palettes
  patchwork          # The Composer of Plots # The Composer of Plots # The Composer of Plots
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 12,
  height = 8,
  units  = "in",
  dpi    = 320)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
raw_data <- read_csv("2025/Ex_055/swdexercise055.csv") |>
  clean_names()


## 3. EXAMINE THE DATA ----
glimpse(raw_data)


## 4. TIDY ----

# P1. Market Share Data ----

# Transform raw data to long format with company identifiers
long_data <- raw_data |>
  # Remove the total market row
  filter(x1 != "Total Market") |>
  # Convert from wide to long format
  pivot_longer(
    cols = c(true_nut_sales, nut_brite_sales, golden_spread_sales),
    names_to = "company", 
    values_to = "sales"
  ) |>
  # Clean company names and calculate percentages
  mutate(
    # Use case_when instead of recode for better readability
    company = case_when(
      company == "true_nut_sales" ~ "TrueNut",
      company == "nut_brite_sales" ~ "NutBrite",
      company == "golden_spread_sales" ~ "GoldenSpread",
      TRUE ~ company  # Fallback for unexpected values
    ),
    # Calculate percentage of category total
    percentage = sales / total_sales * 100
  )

# Calculate overall company totals
company_totals <- raw_data |>
  # Sum sales for each company
  summarise(
    TrueNut = sum(true_nut_sales),
    NutBrite = sum(nut_brite_sales),
    GoldenSpread = sum(golden_spread_sales)
  ) |>
  # Convert to long format
  pivot_longer(
    cols = everything(), 
    names_to = "company", 
    values_to = "sales"
  ) |>
  # Calculate market share percentages
  mutate(percentage = sales / sum(sales) * 100)

# Calculate category coverage for each company
coverage <- long_data |>
  # Only count categories where the company has sales
  filter(sales > 0) |>
  # Count unique categories per company
  group_by(company) |>
  summarise(
    categories_covered = n_distinct(x1),
    # Calculate as percentage of all categories
    coverage_percent = categories_covered / n_distinct(long_data$x1) * 100
  )

# Join company totals with coverage data
position_data <- left_join(company_totals, coverage, by = "company")

# Define quadrant labels with semantic positioning
quadrant_labels <- tibble(
  # Define quadrant centers
  quadrant = c("Specialists", "Limited Players", "Market Leaders", "Volume Players"),
  x = c(25, 25, 75, 75),     
  y = c(65, 20, 65, 20),
  # Add clear descriptions
  description = c(
    "(Niche categories, strong coverage)",
    "(Low share, few categories)",
    "(Strong share, wide coverage)",
    "(High share, category focused)"
  )
) |>
  # Convert to label format expected by ggplot
  rename(label = quadrant)

# P2. Market by Product Category Data ----

# Calculate category statistics and establish ordering
category_stats <- long_data |> 
  # Group by category
  group_by(x1) |>
  # Get key category metrics (just once per category)
  summarize(
    total_sales = first(total_sales),
    .groups = "drop"  
  ) |>
  # Sort by total sales descending
  arrange(desc(total_sales))

# Create ordered factor for consistent category display
category_order <- category_stats |> pull(x1)

# Prepare the main plotting data with ordered categories
plot_data <- long_data |>
  # Create ordered factor with categories in descending sales order
  mutate(
    # Reverse for bottom-to-top ordering in the plot
    x1 = factor(x1, levels = rev(category_order))
  ) |>
  # Only include meaningful sales values
  filter(sales > 0.01)  

# Create the label data from filtered plot data
label_data <- plot_data |> 
  # Format sales values as currency with millions indicator
  mutate(
    label = paste0("$", round(sales, 2), "M")
  )

# Simplified category totals reference (using earlier calculation)
category_totals <- category_stats |>
  # Apply the same factor ordering
  mutate(x1 = factor(x1, levels = rev(category_order)))



## 5. VISUALIZATION ---- 
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = paletteer:::paletteer_d("ltc::trio4")
)

### |-  titles and caption ----
title_text <- str_glue("TrueNut's Market Dominance in Powdered Nut Butter")

subtitle_text <- str_glue("Strategic position and category performance across a $386M market")

# Create caption
caption_text <- create_swd_exe_caption(
  year = 2025,
  month = "Feb",
  source_text =  "Let's Practice! Exercise 5.5"
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
    plot.title = element_text(face = "bold", family = fonts$title, size = rel(1.14), margin = margin(b = 10)),
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

# P1. Market Position Chart ----
p1 <- ggplot(position_data, aes(x = percentage, y = coverage_percent)) +
  # Geoms
  geom_rect(xmin = 0, xmax = 50, ymin = 0, ymax = 50, fill = "gray95", alpha = 0.5) +
  geom_rect(xmin = 50, xmax = 100, ymin = 0, ymax = 50, fill = "gray95", alpha = 0.5) +
  geom_rect(xmin = 0, xmax = 50, ymin = 50, ymax = 100, fill = "gray95", alpha = 0.5) +
  geom_rect(xmin = 50, xmax = 100, ymin = 50, ymax = 100, fill = "gray95", alpha = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = company), size = 6, alpha = 0.8) +
  geom_text(aes(label = company), color = "gray20", 
            fontface = "bold", size = 3.5, vjust = 3) +
  geom_text(
    data = quadrant_labels,
    aes(x = x, y = y, label = label),
    size = 4,
    color = "gray40",
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text(
    data = quadrant_labels,
    aes(x = x, y = y - 4, label = description),  
    size = 3,
    color = "gray50",
    fontface = "italic",
    hjust = 0.5,
    vjust = 0.5
  ) +
  # Scales
  scale_x_continuous(
    labels = percent_format(scale = 1), limits = c(0, 100)
    ) +
  scale_y_continuous(
    labels = percent_format(scale = 1), limits = c(0, 100)
    ) +
  scale_color_manual(values = colors$palette) +
  coord_cartesian(clip = "off") +
  # Labs  
  labs(
    title = "Company Market Position Analysis",
    subtitle = "Comparison of market share vs. category coverage",
    x = "Market Share\n(% of total sales)",
    y = "Category Coverage\n(% of product categories)"
  ) 

# P2. Market by Product Category Chart ----
p2 <- ggplot() +
  # Geoms
  geom_segment(
    data = category_totals,
    aes(y = x1, yend = x1, x = 0, xend = max(total_sales) * 1.05),
    color = "gray85", linewidth = 0.5
  ) +
  geom_point(
    data = plot_data,
    aes(x = sales, y = x1, color = company),
    size = 4, alpha = 0.9,
    show.legend = TRUE
  ) +
  geom_text(
    data = label_data,
    aes(x = sales, y = x1, label = label),
    vjust = -0.9, 
    size = 3,
    show.legend = FALSE  
  ) +
  geom_text(
    data = category_totals,
    aes(x = max(total_sales) * 1.1, y = x1, label = paste0("Total: $", total_sales, "M")),
    hjust = 0, vjust = 0.3, size = 3, color = "gray30",
    show.legend = FALSE
  ) +
  # Scales
  scale_x_continuous(
    labels = dollar_format(suffix = "M"),
    limits = c(-10, max(category_totals$total_sales) * 1.3),
    expand = c(0.01, 0)
  ) +
  scale_color_manual(values = colors$palette) +
  # Labs
  labs(
    title = "Nut Butter Market Analysis by Product Category",
    subtitle = "Sales comparison across product categories by company",
    x = "Sales ($ Millions)",
    y = NULL,
  ) +
  # Theme
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),  
  )

# Combined Charts -----

# Combined Plot -----
combined_plot <- (p1 + p2) +
  plot_layout(widths = c(1, 1))   

combined_plot <- combined_plot +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text( 
        size = rel(2.2),
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

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-27
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────
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
# P paletteer   * 1.6.0    2024-01-21 [?] CRAN (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# P patchwork   * 1.2.0    2024-01-08 [?] CRAN (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P prismatic     1.1.2    2024-04-10 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P rematch2      2.1.2    2020-05-01 [?] CRAN (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────────────────────
