

# Challenge:  #SWDchallenge 2025-- January
## Topic:     visualize qualitative data
## Data:      Gabriel Garcia Marquez "One Hundred Years of Solitude" book reviews
## Author:    Steven Ponce
## Date:      2025-01-05


## 0. DATA SOURCE ----
#' 
#' Book reviews for Gabriel Garcia Marquez book "One Hundred Years of Solitude". 
#' 
#' The data was scrapped from: 
#' (1) goodreads: https://www.goodreads.com/book/show/320.One_Hundred_Years_of_Solitude?from_search=true&from_srp=true&qid=1GomKIPmtF&rank=1
#' (2) librarything: https://www.librarything.com/work/5864
#' 
#' For the web scrapping scripts, refer to `goodread_web_scrapping.R` and `librarything_web_scrapping.R`.
#' 
#' I was unable to scrape reviews from Amazon and Barns & Noble.


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
  skimr,             # Compact and Flexible Summaries of Data
  camcorder,         # Record Your Plot History
  textcat,           # N-Gram Based Text Categorization
  ggdist,            # Visualizations of Distributions and Uncertainty # Visualizations of Distributions and Uncertainty # Visualizations of Distributions and Uncertainty
  tidytext,          # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools
  patchwork          # The Composer of Plots # The Composer of Plots # The Composer of Plots
) 

### |- figure size ---- 
gg_record( 
  dir    = here::here("temp_plots"), 
  device = "png",
  width  = 12,
  height = 12,
  units  = "in",
  dpi    = 320)

### |- resolution ---- 
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
goodreads <- read_csv("2025/01_Jan/goodreads_reviews_full.csv")
librarything <- read_csv("2025/01_Jan/librarything_reviews_full.csv")

# Combine the datasets
combined_reviews <- bind_rows(goodreads, librarything)


## 3. EXAMINE THE DATA ----
glimpse(goodreads)
glimpse(librarything)
glimpse(combined_reviews)


## 4. TIDY ----
combined_reviews_clean <- combined_reviews |>
  # Combine 'star_rating' and 'numeric_rating' into a single 'rating' column
  mutate(rating = coalesce(star_rating, numeric_rating)) |>
  # Convert 'review_date' to Date format
  mutate(review_date = lubridate::mdy(review_date)) |>
  # Standardize column names
  rename(
    reviewer = reviewer_name,
    date = review_date,
    text = review_text
  ) |>
  # Clean up review text  
  mutate(
    text = str_squish(text), # Remove extra whitespace
    text = tolower(text),    # Convert to lowercase
    text = str_replace_all(text, "[^a-zA-Z0-9 .,!?']", "") # Remove special characters
  ) |>
  # Select and reorder columns
  select(reviewer, date, rating, text, source) |>
  # Remove duplicate rows
  distinct() |> 
  mutate(
    language = textcat(text),             # Add detected language as a new column
    word_count = str_count(text, "\\S+")  # Count words in text
    ) |>  
  filter(language == "english") # Keep only English reviews


# Housekeeping
rm(goodreads, librarything, combined_reviews)


# Prepare text data for sentiment analysis
review_sentiments <- combined_reviews_clean |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |>
  inner_join(get_sentiments("nrc")) |>
  # Add rating categories for comparison
  mutate(rating_category = case_when(
    rating <= 2 ~ "Negative (1-2)",
    rating == 3 ~ "Neutral (3)",
    rating >= 4 ~ "Positive (4-5)"
  ))

# 1. Revised Complexity Analysis
complexity_analysis <- combined_reviews_clean |>    
  mutate(
    sentences = str_count(text, "[.!?]+"),
    words_per_sentence = word_count / sentences,
    rating_category = factor(case_when(
      rating <= 2 ~ "Negative (1-2)",
      rating == 3 ~ "Neutral (3)",
      rating >= 4 ~ "Positive (4-5)"
    ), levels = c("Negative (1-2)", "Neutral (3)", "Positive (4-5)"))
  ) |>
  filter(is.finite(words_per_sentence))

# 2. Sentiment Flow (keeping existing structure, updating colors)
sentiment_flow <- review_sentiments |>
  mutate(
    theme = case_when(
      sentiment %in% c("joy", "trust", "anticipation") ~ "positive",
      sentiment %in% c("anger", "fear", "disgust") ~ "negative",
      TRUE ~ "neutral"
    )
  ) |>
  count(rating_category, theme) |>
  group_by(rating_category) |>
  mutate(prop = n/sum(n)) |>
  ungroup()

# 3. Temporal Pattern 
temporal_pattern <- review_sentiments |>
  group_by(reviewer) |>
  mutate(
    position = row_number(),
    position_pct = position/n()
  ) |>
  count(position_pct = round(position_pct, 2), sentiment) |> 
  ungroup()

# 4. Simplified Bigram Network
bigram_graph <- combined_reviews_clean |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, c("word1", "word2"), sep = " ") |>
  filter(
    !word1 %in% stop_words$word,
    !word2 %in% stop_words$word,
    !is.na(word1),
    !is.na(word2)
  ) |>
  count(word1, word2, sort = TRUE) |>
  filter(n >= 4) |>  # Increased threshold
  slice_head(n = 15)  # Take only top 15 pairs



## 5. VISUALIZATION ---- 

### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <-  c(
    "negative" = "#E69B95", 
    "neutral" = "#709BB0",  
    "positive" = "#86B8B1"  
  )

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Jan 2025 &bull; Source: Source: Scrapped from goodreads & librarthing<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

title_text   <- str_glue("From Magic to Mixed Feelings: Analyzing 'One Hundred Years of Solitude' Reviews") 

subtitle_text <- str_glue(
  "How readers experience the novel: A deep dive into emotional responses, writing complexity, and thematic\n
connections across different rating categories",
  
  "\n\n**Note**: This analysis is based on a small sample of 42 reviews, collected from Goodreads and LibraryThing\n
as of January 3, 2025.")

caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

# |- fonts ----
font_add("fa6-brands", here::here("fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf"))
font_add_google("Oswald", regular.wt = 400, family = "title")
font_add_google("Merriweather Sans", regular.wt = 400, family = "subtitle")
font_add_google("Merriweather Sans", regular.wt = 400, family = "text")
font_add_google("Noto Sans", regular.wt = 400, family = "caption")
showtext_auto(enable = TRUE)

### |-  plot theme ----
theme_set(theme_minimal(base_size = 14, base_family = "text"))                

theme_update(
  plot.title.position   = "plot",
  plot.caption.position = "plot",
  legend.position       = "plot",
  plot.background       = element_rect(fill = bkg_col, color = bkg_col),
  panel.background      = element_rect(fill = bkg_col, color = bkg_col),
  plot.margin           = margin(t = 10, r = 20, b = 10, l = 20),
  axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(1.1), 
                                       color = text_col, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.8), color = text_col, family = "text"),
  axis.line.x           = element_line(color = "#252525", linewidth = .3),
  axis.ticks.x          = element_line(color = text_col),  
  axis.title            = element_text(face = "bold"),
  panel.grid.minor      = element_blank(),
  panel.grid.major      = element_blank(),
  panel.grid.major.y    = element_line(color = "grey85", linewidth = .4),
)  

# 1. Sentiment Flow Plot
p1 <- sentiment_flow |>  
  ggplot(aes(x = rating_category, y = prop, fill = theme)) +
  geom_col(position = "fill", alpha = 0.9) +
  scale_fill_manual(values = col_palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "<b>Distribution of Emotional Content by Rating</b>",
    fill = "Emotional Theme",
    x = "Rating Category",
    y = "Proportion of Emotions",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(size = rel(1)),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

# 2. Temporal Pattern Plot
p2 <- temporal_pattern |> 
  ggplot(aes(x = position_pct, y = n, fill = sentiment)) +
  geom_area(position = "fill", alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = c(0, 0)
  ) +
  labs(
    title = "<b>Emotional Flow Through Reviews</b>",
    x = "Relative Position in Review",
    y = "Proportion of Emotions",
    fill = "Emotion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(size = rel(1)),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

# 3. Complexity Analysis Plot
p3 <- complexity_analysis |> 
  ggplot(aes(x = words_per_sentence, y = rating_category, fill = rating_category)) +
  stat_gradientinterval(
    aes(color = after_scale(fill)), 
    point_size = 1.2,
    alpha = 0.3,
    point_alpha = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Negative (1-2)" = "#E69B95",
      "Neutral (3)"    = "#709BB0",
      "Positive (4-5)" = "#86B8B1"
    )
  ) +
  labs(
    title = "<b>Review Complexity by Rating</b>",
    x = "Words per Sentence",
    y = NULL,
    fill = "Rating Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(size = rel(1)),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

# 4. Bigram Network Plot
p4 <- bigram_graph |> 
  ggplot(aes(x = word1, y = word2)) +
  geom_point(aes(size = n), color = col_palette["neutral"], alpha = 0.7) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    title = "<b>Common Word Pairs in Reviews</b>",
    size = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(size = rel(1)),
    legend.position = "right",
    axis.text.x = element_text(hjust = 1),
    panel.grid = element_line(color = "grey90"),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

# Combine plots with adjusted spacing
combined_plots <- (p1 + p2) /
  (p3 + p4) +
  plot_layout(heights = c(1, 1.2)) +
  # plot_layout(guides = 'collect') +  
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        family = "title",
        face = "bold", 
        size = rel(1.7),
        color = title_col,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_markdown(
        family = "subtitle",
        size = rel(1.1),
        color = subtitle_col, 
        margin = margin(b = 20),
        lineheight = 1.1
      ),
      plot.caption = element_markdown(
        family = "caption",
        size = 10, 
        color = caption_col,
        margin = margin(t = 20),
        hjust = 0.5,
        lineheight = 1.2
      )
    )
  )


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
# tz       America/La_Paz
# date     2025-01-04
# rstudio  2024.09.1+394 Cranberry Hibiscus (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────
# ! package        * version  date (UTC) lib source
# V base           * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# P base64enc        0.1-3    2015-07-28 [?] CRAN (R 4.4.0)
# P bit              4.0.5    2022-11-15 [?] CRAN (R 4.4.0)
# P bit64            4.0.5    2020-08-30 [?] CRAN (R 4.4.0)
# P camcorder      * 0.1.0    2022-10-03 [?] CRAN (R 4.4.0)
# P cli              3.6.3    2024-06-21 [?] CRAN (R 4.4.1)
# colorspace       2.1-0    2023-01-23 [1] CRAN (R 4.4.0)
# P commonmark       1.9.1    2024-01-30 [?] CRAN (R 4.4.0)
# P compiler         4.4.0    2024-04-24 [?] local
# P crayon           1.5.2    2022-09-29 [?] CRAN (R 4.4.0)
# P curl             5.2.1    2024-03-01 [?] CRAN (R 4.4.0)
# P datasets       * 4.4.0    2024-04-24 [?] local
# P digest           0.6.35   2024-03-11 [?] CRAN (R 4.4.0)
# P distributional   0.5.0    2024-09-17 [?] CRAN (R 4.4.2)
# dplyr          * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
# P fansi            1.0.6    2023-12-08 [?] CRAN (R 4.4.0)
# farver           2.1.1    2022-07-06 [1] CRAN (R 4.4.0)
# P fastmap          1.1.1    2023-02-24 [?] CRAN (R 4.4.0)
# forcats        * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
# P fs               1.6.4    2024-04-25 [?] CRAN (R 4.4.0)
# generics         0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# P ggdist         * 3.3.2    2024-03-05 [?] CRAN (R 4.4.0)
# ggplot2        * 3.5.1    2024-04-23 [1] CRAN (R 4.4.0)
# P ggtext         * 0.1.2    2022-09-16 [?] CRAN (R 4.4.0)
# P gifski           1.12.0-2 2023-08-12 [?] CRAN (R 4.4.0)
# P glue           * 1.8.0    2024-09-30 [?] CRAN (R 4.4.1)
# P graphics       * 4.4.0    2024-04-24 [?] local
# P grDevices      * 4.4.0    2024-04-24 [?] local
# P grid             4.4.0    2024-04-24 [?] local
# P gridtext         0.1.5    2022-09-16 [?] CRAN (R 4.4.0)
# gtable           0.3.5    2024-04-22 [1] CRAN (R 4.4.0)
# P here           * 1.0.1    2020-12-13 [?] CRAN (R 4.4.0)
# P hms              1.1.3    2023-03-21 [?] CRAN (R 4.4.0)
# P htmltools        0.5.8.1  2024-04-04 [?] CRAN (R 4.4.0)
# P janeaustenr      1.0.0    2022-08-26 [?] CRAN (R 4.4.0)
# P janitor        * 2.2.0    2023-02-02 [?] CRAN (R 4.4.0)
# P jsonlite         1.8.9    2024-09-20 [?] CRAN (R 4.4.1)
# P knitr            1.46     2024-04-06 [?] CRAN (R 4.4.0)
# labeling         0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# P lattice          0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# P lifecycle        1.0.4    2023-11-07 [?] CRAN (R 4.4.0)
# P lubridate      * 1.9.3    2023-09-27 [?] CRAN (R 4.4.0)
# magick           2.8.3    2024-02-18 [1] CRAN (R 4.4.0)
# P magrittr         2.0.3    2022-03-30 [?] CRAN (R 4.4.0)
# P markdown         1.13     2024-06-04 [?] CRAN (R 4.4.1)
# P Matrix           1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods        * 4.4.0    2024-04-24 [?] local
# munsell          0.5.1    2024-04-01 [1] CRAN (R 4.4.0)
# P pacman         * 0.5.1    2019-03-11 [?] CRAN (R 4.4.0)
# P parallel         4.4.0    2024-04-24 [?] local
# P patchwork      * 1.2.0    2024-01-08 [?] CRAN (R 4.4.0)
# P pillar           1.9.0    2023-03-22 [?] CRAN (R 4.4.0)
# P pkgconfig        2.0.3    2019-09-22 [?] CRAN (R 4.4.0)
# P purrr          * 1.0.2    2023-08-10 [?] CRAN (R 4.4.0)
# P R6               2.5.1    2021-08-19 [?] CRAN (R 4.4.0)
# P ragg             1.3.0    2024-03-13 [?] CRAN (R 4.4.0)
# P rappdirs         0.3.3    2021-01-31 [?] CRAN (R 4.4.0)
# RColorBrewer     1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# P Rcpp             1.0.12   2024-01-09 [?] CRAN (R 4.4.0)
# P readr          * 2.1.5    2024-01-10 [?] CRAN (R 4.4.0)
# renv             1.0.7    2024-04-11 [1] CRAN (R 4.4.0)
# P repr             1.1.7    2024-03-22 [?] CRAN (R 4.4.0)
# P rlang            1.1.4    2024-06-04 [?] CRAN (R 4.4.1)
# P rprojroot        2.0.4    2023-11-05 [?] CRAN (R 4.4.0)
# P rstudioapi       0.16.0   2024-03-24 [?] CRAN (R 4.4.0)
# P rsvg             2.6.0    2023-10-08 [?] CRAN (R 4.4.0)
# scales         * 1.3.0    2023-11-28 [1] CRAN (R 4.4.0)
# P sessioninfo      1.2.2    2021-12-06 [?] CRAN (R 4.4.0)
# P showtext       * 0.9-7    2024-03-02 [?] CRAN (R 4.4.0)
# P showtextdb     * 3.0      2020-06-04 [?] CRAN (R 4.4.0)
# P skimr          * 2.1.5    2022-12-23 [?] CRAN (R 4.4.0)
# P slam             0.1-55   2024-11-13 [?] CRAN (R 4.4.1)
# P snakecase        0.11.1   2023-08-27 [?] CRAN (R 4.4.0)
# P SnowballC        0.7.1    2023-04-25 [?] CRAN (R 4.4.0)
# P stats          * 4.4.0    2024-04-24 [?] local
# P stringi          1.8.3    2023-12-11 [?] CRAN (R 4.4.0)
# P stringr        * 1.5.1    2023-11-14 [?] CRAN (R 4.4.0)
# P svglite          2.1.3    2023-12-08 [?] CRAN (R 4.4.0)
# P sysfonts       * 0.8.9    2024-03-02 [?] CRAN (R 4.4.0)
# P systemfonts      1.1.0    2024-05-15 [?] CRAN (R 4.4.0)
# P tau              0.0-26   2024-10-15 [?] CRAN (R 4.4.2)
# P textcat        * 1.0-9    2024-11-13 [?] CRAN (R 4.4.2)
# P textdata         0.4.5    2024-05-28 [?] CRAN (R 4.4.2)
# P textshaping      0.4.0    2024-05-24 [?] CRAN (R 4.4.0)
# P tibble         * 3.2.1    2023-03-20 [?] CRAN (R 4.4.0)
# tidyr          * 1.3.1    2024-01-24 [1] CRAN (R 4.4.0)
# tidyselect       1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# P tidytext       * 0.4.2    2024-04-10 [?] CRAN (R 4.4.0)
# P tidyverse      * 2.0.0    2023-02-22 [?] CRAN (R 4.4.0)
# P timechange       0.3.0    2024-01-18 [?] CRAN (R 4.4.0)
# P tokenizers       0.3.0    2022-12-22 [?] CRAN (R 4.4.0)
# P tools            4.4.0    2024-04-24 [?] local
# P tzdb             0.4.0    2023-05-12 [?] CRAN (R 4.4.0)
# P utf8             1.2.4    2023-10-22 [?] CRAN (R 4.4.0)
# P utils          * 4.4.0    2024-04-24 [?] local
# P vctrs            0.6.5    2023-12-01 [?] CRAN (R 4.4.0)
# P vroom            1.6.5    2023-12-05 [?] CRAN (R 4.4.0)
# P withr            3.0.0    2024-01-16 [?] CRAN (R 4.4.0)
# P xfun             0.43     2024-03-25 [?] CRAN (R 4.4.0)
# P xml2             1.3.6    2023-12-04 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────
# > 