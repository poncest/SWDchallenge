# 02_after_app/global.R
# Makeover App

# ---- Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  lubridate,         # Working with Dates and Times
  shiny,             # Web Application Framework for R
  shinydashboard,    # Create Dashboards with 'Shiny'
  shinythemes,       # Themes for Shiny
  shinyWidgets,      # Custom Inputs Widgets for Shiny
  DT,                # A Wrapper of the JavaScript Library 'DataTables'
  plotly,            # Create Interactive Web Graphics via 'plotly.js'
  bslib              # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
)   


# ---- Data ----
# relative to app folder
traffic <- read_csv("data/A64_traffic.csv") |>
  clean_names() |>
  mutate(
    report_date = as.Date(report_date),
    hour = hms::as_hms(time_period_ending),
    day_type = if_else(wday(report_date, week_start = 1) <= 5,
      "Weekday", "Weekend"
    ),
    sensor = factor(site_id)
  )

# ---- Helpers ----
pct <- function(x) percent(x, accuracy = 0.1)
num <- function(x) comma(x)

# source(here("2025/09_Sep/02_after_app/R/plots.R"))
source("R/plots.R")
