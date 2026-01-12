## SWDchallenge Contribution Heatmap 
## Author: Steven Ponce
## Purpose: GitHub-style calendar heatmap for README

# 1. LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggtext, showtext, glue, here)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 3,
  units  = "in",
  dpi    = 320
)

# 2. DATA EXTRACTION ----
get_contributions_swd <- function(base_path, year_range = 2023:2025) {
  
  # Find year folders
  year_folders <- list.dirs(base_path, recursive = FALSE, full.names = FALSE)
  years <- year_folders[str_detect(year_folders, "^\\d{4}$")] |> as.integer()
  years <- years[years %in% year_range]
  
  # Extract contributions
  contributions <- map_dfr(years, \(yr) {
    year_path <- file.path(base_path, yr)
    folders <- list.dirs(year_path, recursive = FALSE, full.names = FALSE)
    
    # Monthly challenges: 01_Jan, 02_Feb, etc.
    monthly <- folders[str_detect(folders, "^\\d{2}_[A-Za-z]+")] 
    monthly_df <- tibble(
      year = yr,
      month = as.integer(str_extract(monthly, "^\\d{2}")),
      type = "monthly"
    )
    
    # Exercises: Ex_037, Ex_055, etc.
    exercises <- folders[str_detect(folders, "^Ex_\\d+")]
    exercise_df <- tibble(
      year = yr,
      month = NA_integer_,
      type = "exercise"
    ) |> 
      slice(rep(1, length(exercises)))
    
    bind_rows(monthly_df, exercise_df)
  })
  
  return(contributions)
}

# 3. BUILD CALENDAR GRID ----
build_calendar_grid_monthly <- function(contributions, year_range = 2023:2025) {
  
  current_year <- year(Sys.Date())
  current_month <- month(Sys.Date())
  
  # Monthly contributions only (exercises counted separately)
  monthly_contribs <- contributions |> 
    filter(type == "monthly")
  
  # Full 12-month grid for each year
  full_grid <- expand_grid(
    year = year_range,
    month = 1:12
  ) |> 
    left_join(
      monthly_contribs |> mutate(contributed = TRUE),
      by = c("year", "month")
    ) |> 
    mutate(
      contributed = replace_na(contributed, FALSE),
      # Mark future months as NA
      contributed = if_else(
        year > current_year | (year == current_year & month > current_month),
        NA,
        contributed
      )
    )
  
  return(full_grid)
}

# 4. VISUALIZATION ----
create_heatmap_monthly <- function(calendar_data, contributions) {
  
  # Fonts
  font_add_google("Oswald", "title")
  font_add_google("Inter", "text")
  showtext_auto()
  
  # Colors
  col_contributed <- "#7209b7" 
  col_empty       <- "#1b2129"
  col_future      <- "#0d1117"
  col_text        <- "#7d8590"
  col_title       <- "#e6edf3"
  bkg_col         <- "#0d1117"
  
  # Stats
  monthly_total <- sum(calendar_data$contributed, na.rm = TRUE)
  exercise_total <- contributions |> filter(type == "exercise") |> nrow()
  total <- monthly_total + exercise_total
  years_active <- n_distinct(calendar_data$year)
  
  # Year totals (monthly only for the row labels)
  year_totals <- calendar_data |>
    group_by(year) |>
    summarize(n = sum(contributed, na.rm = TRUE), .groups = "drop")
  
  # Month labels
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Plot
  p <- calendar_data |> 
    mutate(year = factor(year, levels = rev(sort(unique(year))))) |>
    ggplot(aes(x = month, y = year)) +
    
    # Tiles
    geom_tile(
      aes(fill = contributed),
      color = bkg_col,
      linewidth = 0.8,
      width = 0.9,
      height = 0.9
    ) +
    
    # Year count labels on right
    geom_text(
      data = year_totals |> mutate(year = factor(year, levels = rev(sort(unique(year))))),
      aes(x = 13.5, y = year, label = n),
      color = col_text,
      family = "text",
      size = 6,
      hjust = 0
    ) +
    
    # Scales
    scale_fill_manual(
      values = c("TRUE" = col_contributed, "FALSE" = col_empty),
      na.value = col_future,
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month_labels,
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    
    coord_fixed(ratio = 1) +
    
    # Labels
    labs(
      title = "#SWDchallenge Contributions",
      subtitle = glue("{total} visualizations across {years_active} years ({monthly_total} monthly + {exercise_total} exercises)"),
      x = NULL,
      y = NULL
    ) +
    
    # Theme
    theme_minimal(base_family = "text") +
    theme(
      plot.background = element_rect(fill = bkg_col, color = NA),
      panel.background = element_rect(fill = bkg_col, color = NA),
      panel.grid = element_blank(),
      axis.text.x = element_text(color = col_text, size = 20),
      axis.text.y = element_text(color = col_text, size = 20, face = "bold"),
      plot.title = element_text(
        family = "title",
        color = col_title,
        size = 50,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        color = col_text,
        size = 25,
        margin = margin(b = 10)
      ),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  return(p)
}

# 5. EXECUTION ----

# Extract data
df_contributions <- get_contributions_swd(here::here(), year_range = 2023:2025)

# Quick check
cat("=== Monthly Challenges ===\n")
df_contributions |> 
  filter(type == "monthly") |> 
  count(year, name = "monthly") |> 
  print()

cat("\n=== Exercises ===\n")
df_contributions |> 
  filter(type == "exercise") |> 
  count(year, name = "exercises") |> 
  print()

cat("\nTotal:", nrow(df_contributions), "\n")

# Build grid and plot
df_grid <- build_calendar_grid_monthly(df_contributions, year_range = 2023:2025)
viz <- create_heatmap_monthly(df_grid, df_contributions)

# Display
print(viz)

# Save
ggsave(
  filename = here::here("summary/swdchallenge_heatmap.png"),
  plot = viz,
  width = 8,
  height = 3,
  dpi = 320,
  bg = "#0d1117"
)