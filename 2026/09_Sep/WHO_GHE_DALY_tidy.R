# WHO Global Health Estimates 2021
# Tidy global DALYs by cause, 2000-2021
# Source: https://www.who.int/data/gho/data/themes/mortality-and-global-health-estimates/global-health-estimates-leading-causes-of-dalys

library(tidyverse)
library(readxl)

# 1. FILE PATHS -----------------------------------------------------------

input_path <- "2026/09_Sep/ghe2021_daly_global_new.xlsx"
output_dir <- "2026/09_Sep/"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

years <- c(2000, 2010, 2015, 2019, 2020, 2021)


# 2. READ ONE YEAR-SHEET --------------------------------------------------

read_daly_sheet <- function(year, path = input_path) {
  sheet_name <- paste("Global", year)

  read_excel(
    path,
    sheet = sheet_name,
    skip = 8,
    col_names = FALSE,
    na = c("", "NA")
  ) |>
    select(1:7) |>
    set_names(
      "code",
      "hierarchy_1",
      "hierarchy_2",
      "hierarchy_3",
      "hierarchy_4",
      "hierarchy_5",
      "dalys"
    ) |>
    mutate(
      code = suppressWarnings(as.integer(code)),
      dalys = suppressWarnings(as.numeric(dalys))
    ) |>
    # Removes the embedded header row; genuine cause rows have numeric codes.
    filter(!is.na(code), !is.na(dalys)) |>
    mutate(
      year = year,

      # WHO stores the hierarchy across five adjacent columns. The final
      # populated cell in each row contains the human-readable cause name.
      cause = coalesce(
        hierarchy_5,
        hierarchy_4,
        hierarchy_3,
        hierarchy_2,
        hierarchy_1
      ),

      # Preserve the source hierarchy. This is descriptive only; it should
      # not yet be used to decide which causes compete in the bump chart.
      cause_level = case_when(
        code == 0L ~ 0L,
        !is.na(hierarchy_5) ~ 4L,
        !is.na(hierarchy_4) ~ 3L,
        !is.na(hierarchy_3) ~ 2L,
        !is.na(hierarchy_2) ~ 1L,
        TRUE ~ NA_integer_
      ),

      dalys_thousands = dalys / 1e3,
      dalys_millions = dalys / 1e6
    ) |>
    select(
      year,
      code,
      cause,
      cause_level,
      dalys,
      dalys_thousands,
      dalys_millions,
      starts_with("hierarchy_")
    )
}


# 3. COMBINE ALL SIX PERIODS ---------------------------------------------

daly_tidy <- map_dfr(years, read_daly_sheet) |>
  group_by(year) |>
  mutate(
    total_dalys = dalys[code == 0L],
    pct_all_dalys = dalys / total_dalys
  ) |>
  ungroup() |>
  arrange(year, code)


# 4. QUALITY CHECKS -------------------------------------------------------

stopifnot(
  setequal(unique(daly_tidy$year), years),
  nrow(daly_tidy) == 215L * length(years),
  n_distinct(daly_tidy$code) == 215L,
  all(count(daly_tidy, year)$n == 215L),
  all(count(daly_tidy, year, code)$n == 1L),
  all(filter(daly_tidy, code == 0L)$pct_all_dalys == 1)
)

cause_dictionary <- daly_tidy |>
  select(
    code,
    cause,
    cause_level,
    starts_with("hierarchy_")
  ) |>
  distinct() |>
  arrange(code)

year_check <- daly_tidy |>
  group_by(year) |>
  summarise(
    causes = n(),
    all_cause_dalys_millions = dalys_millions[code == 0L],
    .groups = "drop"
  )

print(year_check)


# 5. EXPORT ---------------------------------------------------------------

write_csv(
  daly_tidy,
  file.path(output_dir, "who_ghe_daly_global_tidy.csv")
)

write_csv(
  cause_dictionary,
  file.path(output_dir, "who_ghe_daly_cause_dictionary.csv")
)


# IMPORTANT ---------------------------------------------------------------
# Do not rank every row in `daly_tidy` together. The workbook mixes parent
# categories and their children, so doing so would double-count overlapping
# disease burden. The next analytical step is to reconstruct WHO's comparable
# ranking-cause universe and then calculate ranks within each year.
