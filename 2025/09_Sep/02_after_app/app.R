# 02_after_app/app.R
# Makeover App

#' App choices:
#' One page with a small sidebar.
#' 3 controls only: date range, sensor (single/multi), weekday/weekend toggle.
#' 3 KPIs: Avg daily volume, Median speed, % large vehicles.
#' 2 charts: (A) Daily total volume (main) with smoothed trend; (B) Weekday vs Weekend hourly profile.


# source(here::here(("2025/09_Sep/02_after_app/global.R")))
source("global.R")

thematic::thematic_on()

theme <- bs_theme(
  version = 5,
  preset = "bootstrap",
  # base_font = font_google("Inter"),
  # heading_font = font_google("Inter Tight")
)

ui <- page_sidebar(
  theme = theme,
  title = "A64 Traffic — After",
  sidebar = sidebar(
    width = 300,
    helpText("Focus the view with a few simple controls."),
    dateRangeInput(
      "date_range", "Date range",
      start = min(traffic$report_date),
      end = max(traffic$report_date)
    ),
    pickerInput(
      "sensor_pick", "Sensor(s)",
      choices = levels(traffic$sensor), multiple = TRUE,
      selected = levels(traffic$sensor)
    ),
    prettySwitch("wknd_on", "Include Weekends", value = TRUE, status = "info"),
    hr(),
    actionLink("show_data", "View data table", icon = icon("table"))
  ),
  # --- Main content ---
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "Avg daily volume", value = textOutput("kpi_vol")),
    value_box(title = "Median speed", value = textOutput("kpi_spd")),
    value_box(title = "% large vehicles", value = textOutput("kpi_large"))
  ),
  card(
    full_screen = TRUE,
    card_header("Overall pattern"),
    card_body(plotOutput("p_daily", height = 280))
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Weekday vs Weekend"),
      card_body(plotOutput("p_profile", height = 260))
    ),
    card(
      full_screen = TRUE,
      card_header("Sensors at a glance"),
      card_body(plotOutput("p_speedmini", height = 260))
    )
  ),
  tags$footer(
    class = "text-muted small mt-4",
    "#SWDchallenge (Sep 2025) • Data: A64, May 2021 • Source: WebTRIS • Built in Shiny + bslib • © Steven Ponce"
  )
)

server <- function(input, output, session) {
  filt <- reactive({
    d <- traffic |>
      filter(
        report_date >= input$date_range[1],
        report_date <= input$date_range[2],
        sensor %in% input$sensor_pick
      )
    if (!isTRUE(input$wknd_on)) d <- d |> filter(day_type == "Weekday")
    d
  })

  # --- KPIs ---
  output$kpi_vol <- renderText({
    d <- filt() |>
      group_by(report_date) |>
      summarise(dv = sum(total_volume, na.rm = TRUE), .groups = "drop")
    num(round(mean(d$dv, na.rm = TRUE)))
  })

  output$kpi_spd <- renderText({
    spd <- median(filt()$avg_mph, na.rm = TRUE)
    paste0(round(spd, 1), " mph")
  })

  output$kpi_large <- renderText({
    share <- sum(filt()$x1160_cm, na.rm = TRUE) / sum(filt()$total_volume, na.rm = TRUE)
    pct(share)
  })

  # --- Plots ---
  output$p_daily <- renderPlot({
    plot_daily_volume_clean(filt())
  })
  output$p_profile <- renderPlot({
    plot_weekday_weekend_profile(filt())
  })
  output$p_speedmini <- renderPlot({
    # One-row spark faceting: limit to up to 3 sensors for readability
    s <- head(unique(filt()$sensor), 3)
    plot_sensor_speed_spark(filt() |> filter(sensor %in% s))
  })

  # --- Modal data table (on demand only) ---
  observeEvent(input$show_data, {
    showModal(modalDialog(
      title = "Filtered data",
      size = "l",
      easyClose = TRUE,
      DTOutput("tbl")
    ))
  })
  output$tbl <- renderDT({
    filt() |> datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
