
# Challenge:  #SWDchallenge 2025 -- June
## Topic:     transform a graph
## Author:    Steven Ponce
## Date:      2025-06-01


## Original Slides
# XYZ Products: Strategic Sourcing Plan Document
# https://docs.google.com/presentation/d/1zqxsqSE9hfAGQorzDLyVsBU3gltY4Cz1/edit?slide=id.p1#slide=id.p1

## Source Data
# https://docs.google.com/spreadsheets/d/12YGQSg6C50wE-QI8L7F9Ghpeu-sNXH4U/edit?gid=2041126443#gid=2041126443


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Easily Install and Load the 'Tidyverse'
  ggtext,       # Improved Text Rendering Support for 'ggplot2'
  showtext,     # Using Fonts More Easily in R Graphs
  scales,       # Scale Functions for Visualization
  glue          # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
market_share_raw <- readxl::read_excel("2025/06_Jun/Supplier Sourcing Data.xlsx",
  sheet = "market_share",
  skip = 2, trim_ws = TRUE
) |>
  janitor::clean_names()

cost_over_time_raw <- readxl::read_excel("2025/06_Jun/Supplier Sourcing Data.xlsx",
  sheet = "cost_over_time",
  skip = 2, trim_ws = TRUE
) |>
  janitor::clean_names()

spend_by_facility_raw <- readxl::read_excel("2025/06_Jun/Supplier Sourcing Data.xlsx",
  sheet = "by_facility",
  skip = 2, trim_ws = TRUE
) |>
  janitor::clean_names()

evaluations_results_raw <- readxl::read_excel("2025/06_Jun/Supplier Sourcing Data.xlsx",
  sheet = "results",
  skip = 2, trim_ws = TRUE
) |>
  janitor::clean_names()



## 3. EXAMINE THE DATA ----
glimpse(market_share_raw)
glimpse(cost_over_time_raw)
glimpse(spend_by_facility_raw)
glimpse(evaluations_results_raw)



## 4. TIDY ----
# 1. Clean market share data ----
market_share_clean <- market_share_raw |>
  filter(supplier != "Total Spend") |>
  mutate(
    industry_share = as.numeric(industry),
    us_share = as.numeric(us)
  ) |>
  select(supplier, industry_share, us_share)

# Extract spend information separately
total_spend <- tibble(
  category = c("Industry", "Us"),
  spend = c("$2.8M", "~$50M"),
  spend_numeric = c(2.8, 50)
)

# 2. Clean cost over time data ----
years <- c("2022", "2023", "2024", "2025", "2026", "2027", "2028")

# Clean the supplier cost data (rows 2-6)
supplier_costs <- cost_over_time_raw |>
  slice(2:6) |>
  rename(supplier = x1) |>
  mutate(
    `2025` = case_when(
      supplier == "Supplier A" ~ 163910,
      supplier == "Supplier B" ~ 1481647,
      supplier == "Supplier C" ~ 64041,
      supplier == "Supplier D" ~ 1137230,
      supplier == "Total" ~ 2846828,
      TRUE ~ NA_real_
    )
  ) |>
  rename(
    `2022` = actual,
    `2023` = x3,
    `2024` = x4,
    `2026` = forecast,
    `2027` = x7,
    `2028` = x8
  ) |>
  select(-x5) |>
  pivot_longer(cols = -supplier, names_to = "year", values_to = "cost") |>
  mutate(
    year = as.numeric(year),
    cost_millions = cost / 1000000,
    period = case_when(
      year <= 2025 ~ "Actual",
      year >= 2026 ~ "Forecast",
      TRUE ~ NA_character_
    )
  )

# Extract scenario comparison data
scenarios <- cost_over_time_raw |>
  slice(8:10) |>
  select(x5, forecast, x7, x8) |>
  rename(
    scenario = x5,
    `2026` = forecast,
    `2027` = x7,
    `2028` = x8
  ) |>
  # Clean scenario names
  mutate(
    scenario = case_when(
      str_detect(scenario, "Status Quo") ~ "Status Quo",
      str_detect(scenario, "Single") ~ "Single Supplier",
      str_detect(scenario, "Dual") ~ "Dual Supplier",
      TRUE ~ scenario
    )
  ) |>
  pivot_longer(cols = -scenario, names_to = "year", values_to = "cost") |>
  mutate(
    year = as.numeric(year),
    cost_millions = cost / 1000000,
    period = "Forecast"
  ) |>
  filter(!is.na(cost))


# Clean the spend by facility data ----
spend_by_facility_clean <- spend_by_facility_raw |>
  filter(facility != "Grand Total") |>
  pivot_longer(
    cols = starts_with("supplier_"),
    names_to = "supplier",
    values_to = "spend"
  ) |>
  mutate(
    supplier = str_to_upper(str_remove(supplier, "supplier_")),
    spend_thousands = spend / 1000
  )

# Extract totals separately
facility_totals <- spend_by_facility_raw |>
  filter(facility != "Grand Total") |>
  select(facility, grand_total) |>
  mutate(total_thousands = grand_total / 1000)

supplier_totals <- spend_by_facility_raw |>
  filter(facility == "Grand Total") |>
  select(-facility, -grand_total) |>
  pivot_longer(everything(), names_to = "supplier", values_to = "total_spend") |>
  mutate(
    supplier = str_to_upper(str_remove(supplier, "supplier_")),
    total_millions = total_spend / 1000000
  )

# Clean the evaluations data ----
evaluations_clean <- evaluations_results_raw |>
  slice(2:6) |>
  mutate(
    metric = str_extract(test_metric, "^\\d+\\. .+"),
    metric = str_remove(metric, "^\\d+\\. ")
  ) |>
  rename(
    supplier_a = suppliers,
    supplier_b = x3,
    supplier_c = x4,
    supplier_d = x5
  ) |>
  select(metric, supplier_a:supplier_d) |>
  mutate(across(supplier_a:supplier_d, as.numeric)) |>
  pivot_longer(
    cols = starts_with("supplier_"),
    names_to = "supplier",
    values_to = "score"
  ) |>
  mutate(supplier = str_to_upper(str_remove(supplier, "supplier_")))

# Extract averages separately
supplier_averages <- tribble(
  ~supplier, ~avg_score,
  "A", 3.64,
  "B", 4.51,
  "C", 3.72,
  "D", 4.42
)



## 5. VISUALIZATION ---- 

source(here::here("2025/06_Jun/chart_1.R"))
source(here::here("2025/06_Jun/chart_2.R"))
source(here::here("2025/06_Jun/chart_3.R"))

create_benchmark_chart()
create_supplier_performance_chart()
create_strategic_scenario_chart()


# 6. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-06-02
# rstudio  2025.05.0+496 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P crayon        1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] CRAN (R 4.4.2)
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
# labeling      0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] CRAN (R 4.4.2)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] CRAN (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] CRAN (R 4.4.0)
# P R.oo          1.26.0   2024-01-24 [?] CRAN (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg          1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl        1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
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
# P utils       * 4.4.0    2024-04-24 [?] local
# P vctrs         0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] CRAN (R 4.4.2)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 
