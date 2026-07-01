## Challenge: #SWDchallenge 2026 -- July
## Topic:     have a ball: visualize the World Cup
## Author:    Steven Ponce
## Date:      2026-07-XX

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details

## NOTE ON SCOPE: chart is one tile per TOURNAMENT (not per host country).
## 2002 was co-hosted by Japan and South Korea; the tile uses the DEEPER of
## the two co-host results (South Korea, semi-finals) per the "one World Cup
## at a time" framing agreed on before build. Disclose this in the alt text /
## submission post, not on the chart itself -- keeping the chart annotation-free.

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  janitor,    # Simple Tools for Examining and Cleaning Dirty Data
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  camcorder,  # Record Your Plot History
  patchwork   # Compose separate ggplot panels (header/legend, strip, caption)
)

# camcorder::gg_record(
#   dir    = here::here("2026/07_Jul/recording"),
#   device = "png", width = 11, height = 6.5, units = "in", dpi = 320
# )

## 2. READ IN THE DATA ----
## Source: Fjelstul World Cup Database (Joshua C. Fjelstul, Ph.D.)
## https://github.com/jfjelstul/worldcup -- CC-BY-SA 4.0
## Proximate access: cloned from GitHub master branch, data-csv/ folder.
tournaments     <- read_csv(here::here("2026/07_Jul/tournaments.csv"), show_col_types = FALSE) |> clean_names()
host_countries  <- read_csv(here::here("2026/07_Jul/host_countries.csv"), show_col_types = FALSE) |> clean_names()
qualified_teams <- read_csv(here::here("2026/07_Jul/qualified_teams.csv"), show_col_types = FALSE) |> clean_names()

## 3. EXAMINE THE DATA ----
# glimpse(tournaments)
# glimpse(host_countries)
# glimpse(qualified_teams)

# LESSON (classification-lookup-string-verification): never assume the
# `performance` strings -- confirm them before building any lookup map.
qualified_teams |> count(performance)
# -> "third-place match" is hyphenated; "quarter-final" / "quarter-finals"
#    both occur; "final round" (1950) and "second group stage" (1974-82)
#    are early-format equivalents. All handled explicitly below.

# LANDMINE: host_countries has its OWN `performance` column -- a second,
# differently-worded vocabulary ("champions", "runners-up", "quater-finals"
# [sic, missing r]) that is NOT the one used below. Verify it before ever
# relying on it -- it disagrees in wording with qualified_teams and carries
# at least one typo:
host_countries |> count(performance)
# This script deliberately drops host_countries$performance (see the
# distinct() call below, which selects team_id/team_name only) and derives
# tier entirely from qualified_teams$performance + tournaments$host_won,
# which was already verified during the diagnostic session. If you want to
# cross-check tile_data$tier against host_countries$performance as a second
# opinion, that's worth doing once before publishing -- just don't build a
# lookup off the "quater-finals" spelling.

## 4. TIDY ----

### |- men's tournaments only ----
tournaments_clean <- tournaments |>
  mutate(is_womens = str_detect(tournament_name, "Women")) |>
  filter(!is_womens) |>
  select(tournament_id, year)

### |- ordinal performance tier, 0 (group) to 5 (champion) ----
performance_order <- c(
  "group stage"         = 0,
  "second group stage"  = 1.5,
  "round of 16"         = 1,
  "quarter-final"       = 2,
  "quarter-finals"      = 2,
  "final round"         = 2,
  "semi-finals"         = 3,
  "third-place match"   = 3,
  "final"               = 4
)

### |- host teams joined to their tournament finish ----
host_performance <- host_countries |>
  distinct(tournament_id, team_id, team_name) |>
  inner_join(tournaments_clean, by = "tournament_id") |>
  left_join(
    tournaments |> select(tournament_id, host_won),
    by = "tournament_id"
  ) |>
  left_join(
    qualified_teams |> select(tournament_id, team_id, performance),
    by = c("tournament_id", "team_id")
  ) |>
  mutate(
    perf_score = unname(performance_order[performance]),
    tier = case_when(
      host_won == 1                ~ 5,  # champion
      perf_score == 4              ~ 4,  # reached final, runner-up
      perf_score == 3              ~ 3,  # semi-final / third-place match
      perf_score %in% c(2, 1.5)    ~ 2,  # quarter-final / final-round equiv.
      perf_score == 1              ~ 1,  # round of 16
      TRUE                         ~ 0   # group stage
    )
  )

### |- one tile per TOURNAMENT: deepest co-host result wins (see NOTE above) ----
tile_data <- host_performance |>
  summarise(tier = max(tier), .by = c(tournament_id, year)) |>
  arrange(year) |>
  mutate(
    idx  = row_number() - 1,
    ncol = 11,
    col  = idx %% ncol,
    row  = idx %/% ncol,
    # flip so chronologically-earliest row plots on top
    plot_row = max(row) - row,
    # Version B: threshold-compressed color bucket --
    # the story is "reached the final," not "how far exactly"
    color_bucket = case_when(
      tier %in% c(4, 5) ~ "final_or_champion",
      tier == 3         ~ "semi_final",
      tier %in% c(1, 2) ~ "r16_or_qf",
      TRUE               ~ "group_stage"
    )
  )

# France 1998 callout anchor (emerged from the data, not designed in)
callout_tile <- tile_data |> filter(year == 1998)

# small chronological gap right after the France 1998 tile, WITHIN its own
# row only (row 0 stays a continuous line -- everything in it is "before").
# This makes "before/after 1998" readable without the viewer needing to
# understand that the strip wraps row-to-row.
gap_row     <- callout_tile$row
gap_col     <- callout_tile$col
gap_amount  <- 0.4

tile_data <- tile_data |>
  mutate(x_pos = if_else(row == gap_row & col > gap_col, col + gap_amount, col))

## 5. VISUALIZATION ----

### |- plot aesthetics ----
# NOTE: I don't have visibility into get_theme_colors()'s actual internals,
# and my first pass assumed it would pass a 4-category named vector straight
# through to scale_fill_manual(). It didn't (all tiles rendered flat gray).
# A 4-bucket sequential encoding is likely a different shape than what that
# helper was built for (probably your usual single-accent + gray). Bypassing
# it here for the fill scale specifically -- confirm/replace against your
# actual function if this isn't the right call.
tile_colors <- c(
  final_or_champion = "#722F37",
  semi_final         = "#A6717A",
  r16_or_qf          = "#D8B9BD",
  group_stage        = "#E4DFD6"  # was #F2F2F2 -- too close to the page
  # background to read as a tile at all
)

### |- titles and caption ----
# Title A/B'd once more: "Carry" still hinted faintly at causation. "Take"
# tested as more neutral while keeping the same sentence shape. Reviewer
# noted they'd "happily publish either" -- genuinely close call, easy to
# revert to the "Carry" version logged below if you land there instead.
# Alternatives logged:
#   - Hosting the World Cup Doesn't Carry Teams as Far as It Once Did
#   - The Home Crowd Isn't Carrying Hosts Like It Once Did  (original)
#   - World Cup Hosts No Longer Make Deep Runs  (tested as jargon-y)
title_text    <- "Hosting the World Cup Doesn't Take Teams as Far as It Once Did"

# Subtitle tightened: "From 1930 through 2002" is more concrete than
# "Before then," and the em dash gives the second sentence more cadence.
subtitle_text <- glue(
  "No World Cup host has reached the final since France in 1998. From ",
  "1930 through 2002, every host reached the knockout stage — and nearly ",
  "half reached the final."
)

caption_text <- create_swd_caption(
  year = 2026,
  month = "Jul",
  source_text = "Fjelstul World Cup Database (Joshua C. Fjelstul, Ph.D.)"
)

### |- fonts ----
# setup_fonts()
# fonts <- get_font_families()

### |- plot theme ----
# Also unverified: create_base_theme() probably expects your standard
# single-accent + gray shape, not this 4-bucket vector. Passing the accent
# tile color through as a best guess -- confirm against the real signature.
base_theme <- create_base_theme(get_theme_colors(palette = c(accent = "#722F37", neutral = "gray70")))

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.title       = element_text(family = fonts$title, face = "bold", size = 20),
    plot.subtitle    = element_textbox_simple(family = fonts$text, size = 12, margin = margin(b = 16)),
    plot.caption     = element_markdown(family = fonts$caption, size = 8, color = "grey40")
  )
)

theme_set(weekly_theme)

### |- plot ----
col_final <- tile_colors[["final_or_champion"]]

# real-tile legend, own small panel (patchwork) rather than in-strip text
# glyphs -- avoids the alignment/rendering fragility of the earlier attempts
legend_data <- tibble::tibble(
  bucket   = c("final_or_champion", "semi_final", "r16_or_qf", "group_stage"),
  label    = c("Finalist or champion", "Semifinal", "Quarterfinal / R16", "Group stage"),
  # COMPRESSION TEST (per review): tighter spacing, ~60-70% of strip width.
  # Honest caveat: "Finalist (champions included)" is by far the longest
  # label (30 chars) -- it alone eats most of the available compression
  # room, so this lands closer to ~80-85% width than the requested 60-70%,
  # not a full hit on the target. True 60-70% likely needs shortening that
  # label to just "Finalist" (an editorial call, not a layout one -- your
  # call whether the "(champions included)" clarification earns its space).
  # Revert to the wider, previous spacing by swapping this line for:
  #   x_swatch = c(0, 4.6, 7.2, 10.2)
  x_swatch = c(0, 4.4, 6.5, 9.1)
)

### |- panel 1: title + subtitle + real-tile legend ----
p_header <- ggplot() +
  geom_tile(data = legend_data, aes(x = x_swatch, y = 0, fill = bucket),
            width = 0.5, height = 0.5, color = NA) +
  geom_text(data = legend_data, aes(x = x_swatch + 0.45, y = 0, label = label),
            hjust = 0, vjust = 0.5, size = 3, family = fonts$text, color = "grey30") +
  scale_fill_manual(values = tile_colors, guide = "none") +
  coord_cartesian(xlim = c(-0.5, 14), ylim = c(-0.6, 0.6), clip = "off") +
  labs(title = title_text, subtitle = subtitle_text) +
  theme(
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), plot.margin = margin(t = 20, r = 40, b = 4, l = 40)
  )

### |- panel 2: the strip itself ----
p_main <- ggplot(tile_data, aes(x = x_pos, y = plot_row, fill = color_bucket)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.96, height = 0.96) +
  # bookend years -- larger, closer to the strip -- no other axis scaffolding
  annotate("text", x = -0.8, y = max(tile_data$plot_row), label = "1930",
           hjust = 1, size = 4.3, family = fonts$text, color = "grey28") +
  # 2022's tile shifted right by gap_amount (it's after the France gap, in
  # the same row) -- label offset needs to match or it'll float mid-air
  annotate("text", x = 10.8 + gap_amount, y = 0, label = "2022",
           hjust = 0, size = 4.3, family = fonts$text, color = "grey28") +
  # France 1998 sits in the BOTTOM row (plot_row = 0) -- an "above" leader
  # crosses into the top row's tile space, which is exactly what buried it
  # last render. Pointing the leader DOWN instead, into the space that's now
  # open since the legend moved up into p_header. x stays at the unshifted
  # col -- France's own tile doesn't move, only tiles after it do.
  annotate("segment",
           x = callout_tile$col, xend = callout_tile$col,
           y = callout_tile$plot_row - 0.55, yend = callout_tile$plot_row - 1.0,
           linewidth = 0.3, color = col_final, alpha = 0.75) +
  annotate("text",
           x = callout_tile$col, y = callout_tile$plot_row - 1.15,
           label = "Last host finalist\nFrance 1998",
           family = fonts$text, size = 2.3, lineheight = 0.95,
           hjust = 0.5, vjust = 1, color = "grey45") +
  scale_fill_manual(values = tile_colors, guide = "none") +
  coord_equal(clip = "off") +
  theme(
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), plot.title = element_blank(), plot.subtitle = element_blank(),
    plot.margin = margin(t = 4, r = 40, b = 4, l = 40)
  )

### |- panel 3: caption only ----
p_caption <- ggplot() +
  labs(caption = caption_text) +
  theme_void() +
  theme(
    plot.caption = element_markdown(family = fonts$caption, size = 8, color = "grey40"),
    plot.margin  = margin(t = 4, r = 40, b = 10, l = 40)
  )

### |- compose ----
final_plot <- p_header / p_main / p_caption +
  patchwork::plot_layout(heights = c(0.30, 1, 0.08))

# final_plot

snap(final_plot)

## 6. HELPER FUNCTIONS DOCUMENTATION ----
## setup_fonts()          -- initializes showtext fonts for the session
## get_font_families()    -- returns named list of font family strings
## get_theme_colors()     -- validates/returns the named color palette
## create_base_theme()    -- base ggplot2 theme built from the color palette
## extend_weekly_theme()  -- layers challenge-specific theme overrides
## create_swd_caption()   -- builds standardized SWD caption (year + month)
## save_plot()            -- consistent save wrapper, use type = "swd"

## 7. SESSION INFO ----
# sessionInfo()

## save_plot(final_plot, type = "swd", year = 2026, month = "Jul", width = 11, height = 6.5)