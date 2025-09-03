# 01_before_app/R/plots.R
# Baseline (intentionally busy) plotting functions

# Plot daily total volume by sensor (lines colored by sensor)
plot_daily_volume <- function(df) {
  d <- df |>
    group_by(report_date, sensor) |>
    summarise(daily_vol = sum(total_volume, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(report_date, daily_vol, color = sensor, group = sensor)) +
    geom_line(alpha = 0.9, linewidth = 0.9) +
    labs(x = "Date", y = "Daily volume", color = "Sensor")
}

# Plot average hourly volume profile by day type (weekday vs weekend)
plot_daytype_volume <- function(df) {
  d <- df |>
    group_by(day_type, hour) |>
    summarise(vol = mean(total_volume, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(hour, vol, color = day_type)) +
    geom_line(linewidth = 0.9) +
    labs(x = "Hour of day", y = "Avg hourly volume", color = "Day type")
}

# Plot hourly average speed by sensor (faceted to add clutter)
plot_hourly_speed <- function(df) {
  d <- df |>
    group_by(sensor, hour) |>
    summarise(avg_speed = mean(avg_mph, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(hour, avg_speed, group = 1)) +
    geom_line() +
    facet_wrap(~sensor, ncol = 2) +
    labs(x = "Hour of day", y = "Avg speed (mph)")
}

# Plot vehicle-size composition by sensor (stacked share bars)
plot_vehicle_mix <- function(df) {
  d <- df |>
    transmute(
      sensor,
      small = x0_520_cm,
      med1  = x521_660_cm,
      med2  = x661_1160_cm,
      large = x1160_cm
    ) |>
    pivot_longer(small:large, names_to = "class", values_to = "n") |>
    group_by(sensor, class) |>
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")

  ggplot(d, aes(sensor, n, fill = class)) +
    geom_col(position = "fill") +
    labs(x = "Sensor", y = "Share", fill = "Vehicle size")
}
