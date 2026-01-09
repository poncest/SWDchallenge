
## Challenge: #SWDchallenge 2026-- Jan Exercise
## Topic:     presenting a scatterplot
## Author:    Steven Ponce
## Date:      2026-01-09


## 0. DATA SOURCE ----
#' 
#' #' The data can be download at: 
#' https://drive.google.com/file/d/1DUIWDzM3AL-_IEGdftFISm9YM4-j0cQh/view?usp=sharing


## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----   
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue        # Interpreted String Literals
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
manager_data <- readxl::read_excel("2026/Ex_001/EX001 - scatter_plot.xlsx", 
                               trim_ws = TRUE)


## 3. EXAMINE THE DATA ----
glimpse(manager_data)


## 4. TIDY ----

# Calculate reference values
perf_median <- median(manager_data$performance_score)
mgr_median <- median(manager_data$manager_score)

# Add quadrant classification based on medians
manager_data <- manager_data |>
  mutate(
    quadrant = case_when(
      performance_score >= perf_median & manager_score >= mgr_median ~ "strong_both",
      performance_score < perf_median & manager_score >= mgr_median ~ "strong_manager",
      performance_score >= perf_median & manager_score < mgr_median ~ "strong_performer",
      TRUE ~ "development"
    )
  )

# Count per quadrant
manager_data |> count(quadrant)



## 5. VISUALIZATION ---- 

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    bg_color       = "#FFFFFF",
    text_color     = "#252525",
    grid_color     = "gray94",
    point_color    = "#34495E", 
    highlight_red  = "#C0392B",
    highlight_blue = "#2980B9",
    quad_target    = "#F0F7FF", 
    quad_neutral   = "#F9F9F9" 
    
  )
)

### |-  titles and caption ----
title_text <- "Strong results do not always correlate with leadership effectiveness</b>"

subtitle_text <- "While 70% of managers excel in both areas, significant outliers exist where high-performing 
individual<br>contributors struggle to lead their teams effectively."

# Create caption
caption_text <- create_swd_exe_caption(
  year = 2026,
  month = "Jan",
  source_text =  "HR Upward Feedback Survey & Annual Performance Review"
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
    plot.title = element_text(
      face = "bold", family = fonts$title, color = colors$title, 
      size = rel(1.65), margin = margin(b = 10)
      ),
    plot.subtitle = element_text(
      face = "italic",family = fonts$subtitle, color = colors$text, 
      size = rel(0.95), margin = margin(b = 20)
      ),
    plot.caption = element_markdown(
      size = rel(0.65), family = fonts$caption, color = colors$caption,
      hjust  = 0, margin = margin(t = 20), lineheight = 1.4,
    ),
    
    # Axis elements
    axis.title = element_text(color = colors$text, size = rel(0.9)),
    axis.text = element_text(color = colors$text, size = rel(0.8)),
    
    # Grid elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.5),
    
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

### |- Plot ----
ggplot(manager_data, aes(x = performance_score, y = manager_score)) +

  # Quadrant shading
  annotate("rect",
    xmin = perf_median, xmax = 5, ymin = mgr_median, ymax = 10.5,
    fill = "#F4F7F9", alpha = 1
  ) +

  # Geoms
  geom_hline(yintercept = mgr_median, linetype = "dashed", color = "gray80", linewidth = 0.4) +
  geom_vline(xintercept = perf_median, linetype = "dashed", color = "gray80", linewidth = 0.4) +
  geom_point(aes(alpha = quadrant == "strong_both"),
    size = 3.5, shape = 21, fill = colors$palette$point_color, color = "white", stroke = 0.5
  ) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 1.0), guide = "none") +

  # Annotate
  annotate("text",
    x = 4.9, y = 10.2, label = "THE TARGET: High Performers & Strong Leaders",
    family = fonts$text, size = 3.2, color = "#2E86C1", fontface = "bold", hjust = 1
  ) +
  annotate("curve",
    x = 2.0, y = 3.3, xend = 2.3, yend = 1.8, color = colors$palette$highlight_red,
    linewidth = 0.6, curvature = -0.2, arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate("text",
    x = 2.35, y = 1.5, label = "URGENT: Needs support in\nboth results and leadership",
    family = fonts$text, size = 3.5, color = colors$palette$highlight_red, fontface = "bold", hjust = 0, lineheight = 0.9
  ) +
  annotate("curve",
    x = 2.3, y = 8.9, xend = 1.9, yend = 7.5, color = colors$palette$highlight_blue,
    linewidth = 0.6, curvature = 0.2, arrow = arrow(length = unit(0.2, "cm"))
  ) +
  annotate("text",
    x = 1.6, y = 7.2, label = "OPPORTUNITY: Strong leader with\nlow individual output",
    family = fonts$text, size = 3.5, color = colors$palette$highlight_blue, fontface = "bold", hjust = 0, lineheight = 0.9
  ) +

  # Scales
  scale_x_continuous(
    limits = c(1.5, 5), breaks = c(2, 3, 4, 5),
    labels = c("2.0\nNeeds\nImprovement", "3.0\nMeets\nExpectations", "4.0\nExceeds", "5.0\nExceptional")
  ) +
  scale_y_continuous(
    limits = c(0, 10.5), breaks = seq(0, 10, 2),
    labels = c("0", "2", "4", "6", "8\nGood", "10\nExcellent")
  ) +

  # Labs
  labs(
    title = title_text, subtitle = subtitle_text, caption = caption_text,
    x = "Results (Performance Rating)",
    y = "Leadership (Team Feedback Score)"
  ) +

  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.35),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.3,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    ),
  )
  


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #SWDchallenge projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_swd_caption()     - Generate formatted caption with social handles
#    . create_swd_exe_caption()   and #SWDchallenge attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across 50+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/SWDchallenge/tree/main/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/SWDchallenge
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

