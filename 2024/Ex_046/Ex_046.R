
# Challenge:  #SWDchallenge 2024 -- Oct
## Topic:     Exercises | apply emphasis to this table
## Author:    Steven Ponce
## Date:      2024-10-09


## 0. DATA SOURCE ----

## Data:       
##            https://community.storytellingwithdata.com/exercises/apply-emphasis-to-this-table
##            https://drive.google.com/file/d/1nOGd5K2K3-3N6EvzxZzTbi9juM06eUBj/view


## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals
  gt,          # Easily Create Presentation-Ready Display Tables
  readxl       # Read Excel Files
)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ THE DATA ----
sales_data <- read_excel('2024/Ex_046/data_ex_046.xlsx',
                          range = "B8:G18", 
                          trim_ws = TRUE) |> 
  clean_names() |> 
  glimpse()



## 3. EXAMINING THE DATA ----
glimpse(sales_data)  



# 4. VISUALIZATION ---- 

### |- Create a gt table ----
sales_table <- sales_data |>
  gt() |>
  tab_header(
    title = md("**WakeUp Coffee Sales Summary**"),
    subtitle = "Top 10 Accounts by Sales Volume: 4-week sales ending January 31st"
  ) |>
  fmt_currency(
    columns = c(sales_volume, price_per_pound),
    currency = "USD"
  ) |>
  fmt_percent(
    columns = c(percent_change_vs_prior),
    scale_values = TRUE
  ) |>
  fmt_number(
    columns = c(avg_number_of_up_cs),
    decimals = 1
  ) |>
  text_transform(
    locations = cells_body(columns = percent_change_vs_prior),
    fn = function(x) {
      dplyr::case_when(
        x > 0 ~ paste0("\U2191 ", x), # Up arrow for positive values
        x < 0 ~ paste0("\U2193 ", x), # Down arrow for negative values,
        TRUE ~ as.character(x)
      )
    }
  ) |>
  data_color(
    columns = c(percent_change_vs_prior),
    fn = col_numeric(
      domain = c(-0.1, 0.4),
      palette = c("#CA0020", "#F4A582", "#92C5DE", "#0571B0")
    )
  ) |>
  cols_align(
    align = "right",
    columns = c(sales_volume, percent_change_vs_prior, avg_number_of_up_cs, percent_acv_selling, price_per_pound)
  ) |>
  cols_label(
    account = "Account",
    sales_volume = "Sales Volume ($)",
    percent_change_vs_prior = "% Change vs Prior",
    avg_number_of_up_cs = "Avg # of UPCs",
    percent_acv_selling = "% ACV Selling",
    price_per_pound = "Price per Pound ($)"
  ) |>
  tab_options(
    table.font.size = "small",
    data_row.padding = px(5)
  ) |>
  tab_source_note(
    source_note = html("<span style='font-size: 10px;'>UPC is the Universal Product Code, the barcode symbology.<br>ACV is All-Commodity Volume, measured as a percentage from 0 to 100.</span>")
  ) |>
  tab_footnote(
    footnote = md("<span style='font-size: 14px;'>**Account H shows the highest positive growth (+37.90%)**, **Account D has the highest sales volume ($547,265)**, **Accounts J and E show significant declines**</span>"),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = cell_text(font = "Arial"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(columns = everything()),
      cells_body(columns = everything()),
      cells_stub(rows = everything()),
      cells_source_notes()
    )
  ) |>
  tab_style(
    style = cell_text(size = px(24)),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = cell_text(size = px(16)),
    locations = cells_title(groups = "subtitle")
  ) |>
  tab_style(
    style = cell_text(size = px(14)),
    locations = list(
      cells_column_labels(columns = everything()), # Column labels
      cells_body(columns = everything()),          # Data cells
      cells_stub(rows = everything()),             # Row labels (stub)
      cells_source_notes()                         # Source notes
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#D3E4CD"),
      cell_text(color = "black")
    ),
    locations = cells_body(
      rows = account == "H" & percent_change_vs_prior == max(percent_change_vs_prior)
    )
  )

# Print the table
sales_table

# Save the table as a PNG
gtsave(data = sales_table, 
       path = "2024/Ex_046/img/",
       filename = "Ex_046.png"
       )

# 5. SESSION INFO ----  
sessioninfo::session_info(include_base = TRUE) 
 
# ─ Session info ───────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-10-09
# rstudio  2024.09.0+375 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc     0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P camcorder     0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cellranger    1.1.0    2016-07-27 [?] CRAN (R 4.4.0)
# chromote      0.3.1    2024-08-30 [1] CRAN (R 4.4.1)
# P cli           3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace    2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark    1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# P datasets    * 4.4.0    2024-04-24 [?] local
# P digest        0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi         1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver        2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap       1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs            1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext      * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski        1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue        * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# P gridtext      0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# P gt          * 0.11.1   2024-10-04 [?] CRAN (R 4.4.1)
# gtable        0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here          1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms           1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools     0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janitor     * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite      1.8.8    2023-12-04 [?] CRAN (R 4.4.0)
# P knitr         1.46     2024-04-06 [?] CRAN (R 4.4.0)
# P later         1.3.2    2023-12-06 [?] CRAN (R 4.4.0)
# P lifecycle     1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate   * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick        2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr      2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown      1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman        0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P pillar        1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig     2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P processx      3.8.4    2024-03-16 [?] CRAN (R 4.4.0)
# P promises      1.3.0    2024-04-05 [?] CRAN (R 4.4.0)
# P ps            1.7.6    2024-01-18 [?] CRAN (R 4.4.0)
# P purrr       * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6            2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P Rcpp          1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr       * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# P readxl      * 1.4.3    2023-07-06 [?] CRAN (R 4.4.0)
# P rematch       2.0.0    2023-08-30 [?] CRAN (R 4.4.0)
# renv          1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr          1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang         1.1.4    2024-06-04 [?] CRAN (R 4.4.1)
# P rprojroot     2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi    0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg          2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# P sass          0.4.9    2024-03-15 [?] CRAN (R 4.4.0)
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
# webshot2      0.1.1    2023-08-11 [1] CRAN (R 4.4.1)
# websocket     1.4.2    2024-07-22 [1] CRAN (R 4.4.1)
# P withr         3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun          0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2          1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────────
# > 