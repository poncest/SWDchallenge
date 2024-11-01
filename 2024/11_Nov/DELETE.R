## Challenge: #TidyTuesday 2022 week 20 
## Data:      Eurovision Wins
## Author:    Steven Ponce
## Date:      2022-07-12


## 1. Load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  tidytuesdayR,      # Access the Weekly 'TidyTuesday' Project Dataset
)  

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  10,
  height =  8,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

## 2. Read in the data ----
eurovision <- tidytuesdayR::tt_load(2022, week = 20)$eurovision |> 
  clean_names()


## 3. Tidy Data ----

# Add winners for 2023 and 2024
eurovision_additional <- tibble(
  year = c(2023, 2024),
  artist_country = c("Sweden", "Switzerland"),
  winner = TRUE
)

# Combine with original dataset
eurovision <- eurovision |> 
  bind_rows(eurovision_additional) |> 
  clean_names()

# Filter only countries with more than 3 wins
eurovision_summary <- eurovision |> 
  filter(winner == TRUE) |> 
  count(artist_country) |> 
  filter(n > 1)

# Filter the original data to include only those countries
eurovision_filtered <- eurovision |> 
  filter(winner == TRUE & artist_country %in% eurovision_summary$artist_country)

# Create a summary of total wins for each country
eurovision_total_wins <- eurovision_filtered |> 
  count(artist_country, name = "total_wins") |> 
  arrange(desc(total_wins))

# data plot
cumulative_data <- eurovision |> 
  filter(winner == TRUE) |>
  count(year, artist_country) |>
  group_by(artist_country) |>
  mutate(cumulative_wins = cumsum(n))

## 4. Visualization ---- 

### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("peRReo::wyy")[c(2,4)] 
# show_col(col_palette)

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Nov 2024 &bull; Source: TidyTuesday 2023 week 20<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("Eurovision: Sweden and Ukraine Lead with the Most Wins")
subtitle_text <- str_glue("Countries with more than three wins, 1956 - 2024")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

# |- fonts ----
font_add("fa6-brands", "fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
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
  panel.grid.minor      = element_blank(),
  panel.grid.major      = element_blank()
)  

### |- initial plot ----

# Define key countries to highlight
key_countries <- c("Sweden", "Ukraine")

# cumulative_line_chart <- 

# Geoms
ggplot(cumulative_data, 
       aes(x = year, y = cumulative_wins, group = artist_country, color = artist_country)) +
  geom_line(data = cumulative_data |> filter(!artist_country %in% key_countries), 
            linewidth = 0.2, color = "grey40", alpha = 0.65) +
  geom_line(data = cumulative_data |> filter(artist_country %in% key_countries), 
            linewidth = 1.2) +
  geom_point(data = cumulative_data |> filter(artist_country %in% key_countries & year == max(year)),
             aes(color = artist_country), size = 4, shape = 21, fill = "white", stroke = 1.5) +

  
  
  
  ## Challenge: #TidyTuesday 2022 week 20 
  ## Data:      Eurovision Wins
  ## Author:    Steven Ponce
  ## Date:      2022-07-12
  
  
  ## 1. Load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,         # Easily Install and Load the 'Tidyverse'
  ggtext,            # Improved Text Rendering Support for 'ggplot2'
  showtext,          # Using Fonts More Easily in R Graphs
  janitor,           # Simple Tools for Examining and Cleaning Dirty Data
  scales,            # Scale Functions for Visualization
  glue,              # Interpreted String Literals
  here,              # A Simpler Way to Find Your Files
  tidytuesdayR,      # Access the Weekly 'TidyTuesday' Project Dataset
  MetBrewer,         # Color Palettes Inspired by Works at the Metropolitan Museum of Art,
  patchwork,         # Combine Multiple 'ggplot2' Plots
  ggrepel,            # Enhanced Text Label Repulsion
  geomtextpath       # Curved Text in 'ggplot2'
)  

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  10,
  height =  10,
  units  = "in",
  dpi    = 320
)

### |- resolution ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

## 2. Read in the data ----
eurovision <- tidytuesdayR::tt_load(2022, week = 20)$eurovision |> 
  clean_names()


## 3. Tidy Data ----

# Add winners for 2023 and 2024
eurovision_additional <- tibble(
  year = c(2023, 2024),
  artist_country = c("Sweden", "Switzerland"),
  winner = TRUE
)

# Combine with original dataset
eurovision <- eurovision |> 
  bind_rows(eurovision_additional) |> 
  clean_names()

# Filter only countries with more than 3 wins
eurovision_summary <- eurovision |> 
  filter(winner == TRUE) |> 
  count(artist_country) |> 
  filter(n > 3)

# Filter the original data to include only those countries
eurovision_filtered <- eurovision |> 
  filter(winner == TRUE & artist_country %in% eurovision_summary$artist_country)

# Create a summary of total wins for each country
eurovision_total_wins <- eurovision_filtered |> 
  count(artist_country, name = "total_wins") |> 
  arrange(desc(total_wins))

# data plot
cumulative_data <- eurovision |> 
  filter(winner == TRUE) |>
  count(year, artist_country) |>
  group_by(artist_country) |>
  mutate(cumulative_wins = cumsum(n))

## 4. Visualization ---- 

### |- plot aesthetics ----
bkg_col      <- "#f5f5f2"  
title_col    <- "gray20"           
subtitle_col <- "gray20"     
caption_col  <- "gray30"   
text_col     <- "gray20"    
col_palette  <- paletteer::paletteer_d("peRReo::wyy")[c(2,4)] 
# show_col(col_palette)

### |-  titles and caption ----
# icons
tt <- str_glue("#SWDchallenge: Nov 2024 &bull; Source: TidyTuesday 2023 week 20<br>")
li <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa6-brands'>&#xe671; </span>")

# text
title_text    <- str_glue("Eurovision: Sweden and Ukraine Lead with the Most Wins")
subtitle_text <- str_glue("Countries with more than three wins, 1956 - 2024")
caption_text  <- str_glue("{tt} {li} stevenponce &bull; {bs} sponce1 &bull; {gh} poncest &bull; #rstats #ggplot2")

# |- fonts ----
font_add("fa6-brands", "fonts/6.6.0/Font Awesome 6 Brands-Regular-400.otf")
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
  axis.line.x           = element_line(color = "#252525", linewidth = .2),
  panel.grid.minor      = element_blank(),
  panel.grid.major      = element_blank(),
  axis.ticks.x          = element_line(color = text_col),  # Show x-axis ticks
)  

### |- initial plot ----

# Define key countries to highlight
key_countries <- c("Sweden", "Ukraine")

cumulative_line_chart <-
  # Geoms
  ggplot(
    cumulative_data,
    aes(x = year, y = cumulative_wins, group = artist_country, color = artist_country)
  ) +
  geom_line(
    data = cumulative_data |> filter(!artist_country %in% key_countries),
    linewidth = 0.35, color = "grey70", alpha = 0.7
  ) +
  geom_line(
    data = cumulative_data |> filter(artist_country %in% key_countries),
    linewidth = 1.2
  ) +
  geom_point(
    data = cumulative_data |> filter(artist_country %in% key_countries & year == max(year)),
    aes(color = artist_country), size = 4, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_text(
    data = cumulative_data |> filter(artist_country %in% key_countries & year == max(year)),
    aes(label = str_glue("{artist_country} ({cumulative_wins})")),
    hjust = -0.15, size = 4, fontface = "bold"
  ) +

  # Scales
  scale_x_continuous(
    breaks = pretty_breaks(n = 5),
    limits = c(min(cumulative_data$year), max(cumulative_data$year) + 5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 3),
    limits = c(1, 15)
  ) +
  scale_color_manual(values = col_palette) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    x = "Year",
    y = "Cumulative Wins",
    color = "Country",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
      family = "title",
      face = "bold",
      color = title_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size = rel(1.1),
      family = "subtitle",
      color = subtitle_col,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = "caption",
      color = caption_col,
      lineheight = 1.1,
      hjust = 0.5,
      halign = 1,
      margin = margin(t = 15, b = 5)
    )
  )
