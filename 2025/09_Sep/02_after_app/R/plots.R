# 02_after_app/R/plots.R
# Makeover App


# plots.R  (Better app)

# ---- House style ----
theme_swd <- function(base_size = 14, base_family = NULL) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = rel(1.15), color = .swd_col$neutral),
      axis.title = element_text(face = "bold", size = rel(1.0), color = .swd_col$neutral),
      axis.text = element_text(size = rel(0.95), color = .swd_col$neutral),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = rel(1.0), color = .swd_col$neutral),
      strip.background = element_rect(fill = .swd_col$strip_bg, color = NA),
      plot.margin = margin(6, 12, 6, 6)
    )
}

# Neutral + accent palette
.swd_col <- list(
  accent   = "#3B70F2",
  accent2  = "#6E9FFB",
  neutral  = "#4A4A4A",
  neutral2 = "#9AA0A6",
  strip_bg = "#F2F3F5"
)

# Convenience: nice hour labels for hms objects
scale_x_hour_compact <- function(...) {
  scale_x_time(labels = scales::label_time(format = "%H:%M"), ...)
}


# Plot functions

# Plot daily traffic volume with LOESS smoothing (overall trend)
plot_daily_volume_clean <- function(df) {
  d <- df |>
    group_by(report_date) |>
    summarise(daily_vol = sum(total_volume, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(report_date, daily_vol)) +
    geom_line(linewidth = 0.9, color = .swd_col$neutral) +
    geom_smooth(
      method = "loess", se = TRUE, linewidth = 0.9,
      color = .swd_col$accent, fill = .swd_col$accent2, alpha = 0.25
    ) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      title = "Daily traffic volume",
      x = NULL, y = "Daily volume"
    ) +
    theme_swd()
}

# Plot average hourly volume profile comparing Weekday vs Weekend
# (aggregates 15-min bins -> hourly per day -> average across days)
plot_weekday_weekend_profile <- function(df) {
  hourly <- df |>
    mutate(hour_int = lubridate::hour(hour)) |>
    group_by(day_type, report_date, hour_int) |>
    summarise(hourly_vol = sum(total_volume, na.rm = TRUE), .groups = "drop")

  avg_by_hour <- hourly |>
    group_by(day_type, hour_int) |>
    summarise(avg_vol = mean(hourly_vol, na.rm = TRUE), .groups = "drop")

  ggplot(avg_by_hour, aes(hour_int, avg_vol, color = day_type)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("Weekday" = .swd_col$accent, "Weekend" = .swd_col$neutral2)) +
    scale_x_continuous(breaks = seq(0, 23, 4), labels = function(x) sprintf("%02d:00", x)) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, .02))) +
    labs(title = "Weekday vs weekend profile", x = "Hour of day", y = "Avg hourly volume", color = NULL) +
    theme_swd()
}

# Plot small-multiple sparklines of average speed by sensor
plot_sensor_speed_spark <- function(df) {
  d <- df |>
    group_by(sensor, report_date) |>
    summarise(avg_speed = mean(avg_mph, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(report_date, avg_speed)) +
    geom_line(linewidth = 0.8, color = .swd_col$neutral) +
    facet_wrap(~sensor,
      nrow = 1, #scales = "free_y",
      labeller = labeller(sensor = function(x) paste0("Sensor ", x))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    labs(title = "Sensor speed trend (minis)", x = NULL, y = "Avg speed (mph)") +
    theme_swd() +
    theme(
      strip.text = element_text(size = rel(1.05), face = "bold"),
      strip.background = element_rect(fill = .swd_col$strip_bg, color = NA)
    )
}
